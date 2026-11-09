#include "dds_engine.h"

/* Emission continuations live on the heap, independently of the C stack. */
typedef enum { DDS_FRAME_SV, DDS_FRAME_REF, DDS_FRAME_ARRAY, DDS_FRAME_HASH } dds_frame_kind;
typedef enum {
    DDS_STAGE_INITIAL = 0, DDS_STAGE_CHILD, DDS_STAGE_SCALAR_REF
} dds_frame_stage;

/* Frames borrow graph SVs and catalogue roles. name, class_name, keys,
 * hidden and temporary_sv own references released by
 * dds_release_frame. The embedded temporary node lives only as long as
 * this frame and must never be stored in a deferred fixup. */
struct dds_frame {
    struct dds_frame *parent;
    dds_frame_kind kind;
    dds_frame_stage stage;
    SV *sv, *name, *temporary_sv;
    UV depth;
    IV indent;
    dds_mark *mark;
    bool is_ref;
    dds_result result;
    dds_active active;
    dds_node *node, temporary;
    dds_role *role;
    bool add_do;
    SV *class_name, *thaw;
    AV *hidden, *keys;
    bool overloaded, add_bless, add_lock, fix_lock, inline_thaw, function_thaw;
    SSize_t k, last, run;
    UV width;
    IV last_n;
};

static void
dds_push_frame(pTHX_ dds_ctx *ctx, dds_frame_kind kind, SV *sv, UV depth,
               dds_mark *mark, SV *name, IV indent, bool is_ref)
{
    struct dds_frame *frame;
    if (depth > 16384) croak("DDS maximum traversal depth (16384) exceeded");
    if ((frame = ctx->free_frames)) ctx->free_frames = frame->parent;
    else Newx(frame, 1, struct dds_frame);
    Zero(frame, 1, struct dds_frame);
    frame->parent = ctx->frames; ctx->frames = frame;
    frame->kind = kind; frame->sv = sv; frame->depth = depth;
    /* Tied fetches may return mortals created by the parent step. Ordinary
     * graph SVs are borrowed without changing their reference counts. */
    if (SvTEMP(sv)) frame->temporary_sv = SvREFCNT_inc(sv);
    frame->mark = mark; frame->name = SvREFCNT_inc(name); frame->indent = indent; frame->is_ref = is_ref;
    frame->active.parent = ctx->active; frame->active.sv = sv;
    ctx->active = &frame->active;
}

static void
dds_release_frame(pTHX_ struct dds_frame *frame)
{
    /* Only these fields own SV references. Graph values remain borrowed
     * from the live graph or callback keepalive. Clear before release
     * so exception cleanup cannot release the same ownership twice. */
#define RELEASE(field) STMT_START { SV *owned = (SV *)frame->field; frame->field = NULL; SvREFCNT_dec(owned); } STMT_END
    RELEASE(name); RELEASE(class_name); RELEASE(temporary_sv);
    RELEASE(hidden); RELEASE(keys);
#undef RELEASE
}

void
dds_clear_frames(pTHX_ dds_ctx *ctx)
{
    struct dds_frame *frame;
    while ((frame = ctx->frames)) {
        dds_release_frame(aTHX_ frame); ctx->frames = frame->parent; Safefree(frame);
    }
    while ((frame = ctx->free_frames)) {
        dds_release_frame(aTHX_ frame); ctx->free_frames = frame->parent; Safefree(frame);
    }
}

static SV *dds_concat(pTHX_ const char *prefix, SV *value, const char *suffix)
{
    SV *out = DDS_TEMP(newSVpv(prefix, 0));
    if (value) DDS_CAT(out, value);
    sv_catpv(out, suffix);
    return out;
}

dds_mark *
dds_new_mark(pTHX_ dds_ctx *ctx)
{
    dds_mark *mark;
    Newxz(mark, 1, dds_mark);
    mark->next = ctx->marks;
    ctx->marks = mark;
    return mark;
}

dds_fix *
dds_new_fix(pTHX_ dds_fix_kind kind, SV *lhs, dds_node *node)
{
    dds_fix *fix;
    Newxz(fix, 1, dds_fix);
    fix->kind = kind;
    fix->lhs = lhs ? newSVsv(lhs) : NULL;
    fix->node = node;
    return fix;
}

void
dds_add_fix(pTHX_ dds_ctx *ctx, dds_fix *fix)
{
    if (fix->kind == DDS_FIX_VAR) {
        fix->next = ctx->fix_head;
        ctx->fix_head = fix;
        if (!ctx->fix_tail) ctx->fix_tail = fix;
    }
    else {
        if (ctx->fix_tail) ctx->fix_tail->next = fix;
        else ctx->fix_head = fix;
        ctx->fix_tail = fix;
    }
    ctx->fixes++;
}

static void
dds_call_fix(pTHX_ dds_ctx *ctx, const char *function, SV *args, dds_node *source)
{
    dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_CALL, args, source);
    fix->text = newSVpv(function, 0);
    dds_add_fix(aTHX_ ctx, fix);
}

static void
dds_free_fix(pTHX_ dds_fix *fix)
{
    SvREFCNT_dec(fix->lhs); SvREFCNT_dec(fix->text);
    SvREFCNT_dec(fix->class_name); SvREFCNT_dec(fix->args);
    SvREFCNT_dec(fix->glob);
    Safefree(fix);
}

static SV *
dds_ref_expression(pTHX_ dds_ctx *ctx, SV *name, SV *class_name)
{
    const char *p = DDS_PV(name);
    SV *out = DDS_TEMP(newSVpvs(""));
    if (SvOK(class_name)) { sv_catpvs(out, "bless("); DDS_CAT(out, ctx->style.space); }
    if (strchr("@%&", p[0])) sv_catpvs(out, "\\");
    DDS_CAT(out, name);
    if (SvOK(class_name)) {
        sv_catpvs(out, ","); DDS_CAT(out, ctx->style.space);
        DDS_CAT(out, dds_quote(aTHX_ class_name, FALSE));
        DDS_CAT(out, ctx->style.space); sv_catpvs(out, ")");
    }
    return out;
}

static void
dds_lock_fix_output(pTHX_ dds_ctx *ctx, dds_fix *fix)
{
    SSize_t i;
    SV *name = fix->node->ref.name;
    bool hidden = fix->args && av_len(fix->args) >= 0;
    dds_put(aTHX_ ctx, hidden ? "lock_keys_plus( " : "lock_keys( ");
    dds_write(aTHX_ ctx, name);
    if (hidden) {
        dds_put(aTHX_ ctx, ", ");
        for (i = 0; i <= av_len(fix->args); i++) {
            if (i) dds_put(aTHX_ ctx, ", ");
            dds_write(aTHX_ ctx, dds_quote(aTHX_ *av_fetch(fix->args, i, 0), FALSE));
        }
        dds_put(aTHX_ ctx, ");\n");
    }
    else dds_put(aTHX_ ctx, " );\n");
}

static void
dds_thaw_output(pTHX_ dds_ctx *ctx, dds_fix *fix)
{
    SV *name = fix->node->ref.name;
    const char *p = DDS_PV(fix->text), *method = p;
    STRLEN len = strlen(p), i;
    bool simple = TRUE, function = len >= 2 && strEQ(p + len - 2, "()");
    if (method[0] == '-' && method[1] == '>') { method += 2; len -= 2; }
    for (i = 0; i < len - (function ? 2 : 0); i++)
        if (!isALNUMC_A(method[i]) && method[i] != '_' && method[i] != ':') simple = FALSE;
    if (fix->node->ref.done) fix->node->ref.done->ready = TRUE;
    if (!simple) {
        SV *text = DDS_TEMP(newSVsv(fix->text));
        STRLEN n, at;
        const char *s = SvPV(text, n);
        dds_put(aTHX_ ctx, "for ("); dds_write(aTHX_ ctx, name); dds_put(aTHX_ ctx, ") {\n");
        for (at = 0; at < n; at++) {
            if (s[at] == '\t' && (!at || s[at-1] == '\n')) continue;
            dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(s + at, 1)));
        }
        dds_put(aTHX_ ctx, "\n}\n");
    }
    else if (function) {
        dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(method, len - 2)));
        dds_put(aTHX_ ctx, "( "); dds_write(aTHX_ ctx, name); dds_put(aTHX_ ctx, " );\n");
    }
    else {
        dds_write(aTHX_ ctx, name); dds_put(aTHX_ ctx, "->");
        dds_put(aTHX_ ctx, method); dds_put(aTHX_ ctx, "();\n");
    }
}

void
dds_apply_fixes(pTHX_ dds_ctx *ctx, bool final)
{
    bool glob_work, progress;
    do {
        dds_fix **link = &ctx->fix_head;
        dds_fix **glob_tail = &ctx->glob_head;
        glob_work = FALSE;
        progress = FALSE;
        while (*link) {
            dds_fix *fix = *link;
            bool ready = FALSE;
            dds_role *role = fix->node ? (fix->kind == DDS_FIX_SLOT || fix->kind == DDS_FIX_CALL
                              ? &fix->node->slot : &fix->node->ref) : NULL;
            switch (fix->kind) {
                case DDS_FIX_SLOT:
                case DDS_FIX_REF:
                    ready = role && role->done && role->done->ready;
                    if (ready) {
                        SV *lhs = fix->lhs, *rhs = role->name, *underef;
                        if (fix->kind == DDS_FIX_SLOT && (underef = dds_underef(aTHX_ lhs))) {
                            lhs = underef; rhs = dds_concat(aTHX_ "\\", rhs, "");
                        }
                        else if (fix->kind == DDS_FIX_REF && strchr("@%&", DDS_PV(rhs)[0]))
                            rhs = dds_ref_expression(aTHX_ ctx, rhs,
                                      fix->class_name ? fix->class_name : &PL_sv_undef);
                        dds_write(aTHX_ ctx, lhs); dds_put(aTHX_ ctx, " = ");
                        dds_write(aTHX_ ctx, rhs); dds_put(aTHX_ ctx, ";\n");
                        if (fix->mark) fix->mark->ready = TRUE;
                    }
                    break;
                case DDS_FIX_CALL:
                    ready = (!role || (role->done && role->done->ready))
                            && (!strEQ(DDS_PV(fix->text), "weaken") || (final && ctx->final_weakening));
                    if (ready) {
                        dds_write(aTHX_ ctx, fix->text); dds_put(aTHX_ ctx, "(");
                        dds_write(aTHX_ ctx, fix->lhs);
                        if (role) { dds_put(aTHX_ ctx, ", "); dds_write(aTHX_ ctx, role->name); }
                        dds_put(aTHX_ ctx, ");\n");
                    }
                    break;
                case DDS_FIX_BLESS:
                    ready = final;
                    if (ready) {
                        STRLEN len;
                        const char *p = SvPV(ctx->style.bless, len);
                        dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(p, len ? len - 1 : 0)));
                        dds_put(aTHX_ ctx, " "); dds_write(aTHX_ ctx, role->name);
                        dds_put(aTHX_ ctx, ", "); dds_write(aTHX_ ctx, dds_quote(aTHX_ fix->text, FALSE));
                        dds_put(aTHX_ ctx, " ");
                        if (len) dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(p + len - 1, 1)));
                        dds_put(aTHX_ ctx, ";\n");
                    }
                    break;
                case DDS_FIX_LOCK:
                    ready = role && role->done && role->done->ready;
                    if (ready) dds_lock_fix_output(aTHX_ ctx, fix);
                    break;
                case DDS_FIX_THAW:
                    ready = final;
                    if (ready) dds_thaw_output(aTHX_ ctx, fix);
                    break;
                case DDS_FIX_GLOB:
                    ready = TRUE;
                    break;
                case DDS_FIX_VAR:
                    ready = TRUE;
                    if (ctx->style.declare) dds_put(aTHX_ ctx, "my ");
                    dds_write(aTHX_ ctx, fix->lhs); dds_put(aTHX_ ctx, " = ");
                    dds_write(aTHX_ ctx, dds_ref_expression(aTHX_ ctx, fix->text,
                                          fix->class_name ? fix->class_name : &PL_sv_undef));
                    dds_put(aTHX_ ctx, ";\n");
                    break;
            }
            if (!ready) { link = &fix->next; continue; }
            progress = TRUE;
            *link = fix->next;
            if (fix == ctx->fix_tail) ctx->fix_tail = NULL;
            if (fix->kind == DDS_FIX_GLOB) {
                fix->next = NULL; *glob_tail = fix; glob_tail = &fix->next;
            }
            else dds_free_fix(aTHX_ fix);
        }
        if (ctx->fix_head) {
            ctx->fix_tail = ctx->fix_head;
            while (ctx->fix_tail->next) ctx->fix_tail = ctx->fix_tail->next;
        }
        while (ctx->glob_head) {
            dds_fix *fix = ctx->glob_head;
            dds_emit_glob(aTHX_ ctx, fix->glob, fix->lhs, fix->depth);
            ctx->glob_head = fix->next;
            dds_free_fix(aTHX_ fix);
            glob_work = TRUE;
        }
    } while (glob_work || progress);
}

static IV
dds_brace(pTHX_ dds_ctx *ctx, SV *name, char type, bool multiline, IV indent, UV children)
{
    bool open = strchr("{[(", type) != NULL;
    const char *p = DDS_PV(name);
    char brace = p[0] == '%' || p[0] == '@' ? (open ? '(' : ')') : type;
    if (multiline) {
        indent += open ? ctx->style.indentcols : -ctx->style.indentcols;
        if (!open) { dds_put(aTHX_ ctx, "\n"); dds_spaces(aTHX_ ctx, indent); }
        dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(&brace, 1)));
        if (open) { dds_put(aTHX_ ctx, "\n"); dds_spaces(aTHX_ ctx, indent); }
    }
    else {
        if (!open && children) dds_write(aTHX_ ctx, ctx->style.space);
        dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(&brace, 1)));
        if (open && children) dds_write(aTHX_ ctx, ctx->style.space);
    }
    return indent;
}

static bool
dds_rle_eligible(pTHX_ dds_ctx *ctx, SV *sv)
{
    dds_node *node;
    if (SvROK(sv) || SvREADONLY(sv) || SvGMAGICAL(sv)) return FALSE;
    node = dds_candidate(aTHX_ sv) ? dds_find(aTHX_ ctx, sv, FALSE) : NULL;
    return !node || !node->slot.present || node->slot.encounters == 1;
}

static bool
dds_step_array(pTHX_ dds_ctx *ctx, struct dds_frame *f)
{
    AV *av = (AV *)f->sv;
    UV depth = f->depth;
    dds_mark *mark = f->mark;
    SV *name = f->name;
    IV indent = f->indent;
    SSize_t k = 0, last;
    if (f->stage != DDS_STAGE_INITIAL) last = f->last;
    else {
        if (SvTIED_mg((SV *)av, PERL_MAGIC_tied)) dds_callback_guard(aTHX_ ctx);
        last = av_len(av);
    }
    bool multiline = ctx->style.indent && last > 0;
    bool full = ctx->style.indent > 1;
    IV last_n = 0;
    if (f->stage == DDS_STAGE_INITIAL) {
        indent = dds_brace(aTHX_ ctx, name, '[', multiline, indent, last + 1);
        f->indent = indent; f->last = last;
    }
    k = f->k; last_n = f->last_n;
    while (k <= last) {
        SV **entry = f->stage != DDS_STAGE_INITIAL ? NULL : av_fetch(av, k, 0);
        SV *value = entry ? *entry : &PL_sv_undef;
        SSize_t count = 1;
        bool newline;
        dds_result result;
        if (f->stage != DDS_STAGE_INITIAL) { count = f->run; goto child_done; }
        f->active.sv = value;
        if (ctx->style.rle && dds_rle_eligible(aTHX_ ctx, value)) {
            while (k + count <= last) {
                SV **next = av_fetch(av, k + count, 0);
                SV *v = next ? *next : &PL_sv_undef;
                if (!dds_rle_eligible(aTHX_ ctx, v) || cBOOL(SvOK(value)) != cBOOL(SvOK(v))
                    || cBOOL(SvTRUE(value)) != cBOOL(SvTRUE(v))) break;
                if (cBOOL(SvUTF8(value)) != cBOOL(SvUTF8(v))) break;
                if (ctx->style.dualvars) {
                    if (cBOOL(SvNIOK(value)) != cBOOL(SvNIOK(v))) break;
                    if (SvNIOK(value) && SvNIOK(v)) {
                        if (cBOOL(SvIOK(value)) != cBOOL(SvIOK(v))) break;
                        if (SvIOK(value)) {
                            if (cBOOL(SvIsUV(value)) != cBOOL(SvIsUV(v))) break;
                            if (SvIsUV(value) ? SvUV(value) != SvUV(v) : SvIV(value) != SvIV(v)) break;
                        }
                        else if (SvNV(value) != SvNV(v)) break;
                    }
                }
                if (SvOK(value) && sv_cmp(value, v)) break;
                count++;
            }
        }
        newline = multiline && (!last_n || SvROK(value));
        if (k) {
            dds_write(aTHX_ ctx, ctx->style.arysep);
            if (newline) { dds_put(aTHX_ ctx, "\n"); dds_spaces(aTHX_ ctx, indent); ctx->column = 0; }
            else {
                dds_write(aTHX_ ctx, ctx->style.space);
                if (!ctx->style.rle && !SvTRUE(ctx->style.space) && ctx->column > 1024) {
                    dds_put(aTHX_ ctx, "\n"); ctx->column = 0;
                }
            }
        }
        if (count > 1) { dds_put(aTHX_ ctx, "("); dds_write(aTHX_ ctx, ctx->style.space); ctx->column += 2; }
        f->stage = DDS_STAGE_CHILD; f->k = k; f->run = count; f->last_n = last_n;
        dds_push_frame(aTHX_ ctx, DDS_FRAME_SV, value, depth + 1, mark,
                       dds_child_name(aTHX_ name, '[', NULL, k), indent, FALSE);
        return FALSE;
      child_done:
        result = f->result; f->stage = DDS_STAGE_INITIAL;
        last_n = !full && !ctx->do_nl && ctx->column < 60 ? last_n + 1 : 0;
        if (result.alias) {
            SV *args = DDS_TEMP(newSVsv(dds_child_name(aTHX_ name, '@', NULL, 0)));
            sv_catpvf(args, ", %" IVdf, (IV)k);
            dds_call_fix(aTHX_ ctx, "alias_av", args, result.alias);
        }
        if (count > 1) {
            SV *text = DDS_TEMP(newSVsv(ctx->style.space));
            sv_catpvs(text, ")"); DDS_CAT(text, ctx->style.space);
            sv_catpvs(text, "x"); DDS_CAT(text, ctx->style.space);
            sv_catpvf(text, "%" IVdf, (IV)count);
            ctx->column += sv_len_utf8(text); dds_write(aTHX_ ctx, text);
        }
        k += count;
        f->active.sv = (SV *)av;
    }
    dds_brace(aTHX_ ctx, name, ']', multiline, indent, last + 1);
    Zero(&f->result, 1, dds_result);
    return TRUE;
}

static bool
dds_step_hash(pTHX_ dds_ctx *ctx, struct dds_frame *f)
{
    HV *hv = (HV *)f->sv;
    UV depth = f->depth;
    dds_mark *mark = f->mark;
    SV *name = f->name;
    IV indent = f->indent;
    dds_node *node = f->node;
    AV *keys = f->keys;
    if (!keys) {
        keys = dds_keys(aTHX_ ctx, hv, 1, NULL);
        f->keys = (AV *)SvREFCNT_inc((SV *)keys);
    }
    UV count = av_len(keys) + 1, width = 0, sum = 0, max = 0;
    SSize_t k;
    IV child_indent, last_n = 0;
    bool full = ctx->style.indent > 1;
    bool multiline = ctx->style.indent && count > 1;
    if (node && node->key_count) multiline = ctx->style.indent && node->key_count > 1;
    if (f->stage == DDS_STAGE_INITIAL && ctx->style.indentkeys && multiline) {
        if (node && node->key_count) width = node->key_width;
        else {
            for (k = 0; k <= av_len(keys); k++) {
                UV len = sv_len_utf8(dds_quote(aTHX_ *av_fetch(keys, k, 0), TRUE));
                sum += len; if (max < len) max = len;
            }
            width = max > 8 && (2.0/3.0 * max) > ((NV)sum / count)
                    ? (UV)(0.5 + (NV)sum/count) : max;
        }
    }
    if (f->stage == DDS_STAGE_INITIAL) indent = dds_brace(aTHX_ ctx, name, '{', multiline, indent, count);
    else width = f->width;
    child_indent = indent + (width ? width + 2 * sv_len_utf8(ctx->style.space) + sv_len_utf8(ctx->style.hashsep) : 0);
    last_n = f->last_n;
    for (k = f->k; k <= av_len(keys); k++) {
        SV *key = *av_fetch(keys, k, 0), *quoted;
        HE *he;
        SV *value;
        dds_result result;
        if (f->stage != DDS_STAGE_INITIAL) goto child_done;
        if (!SvOK(key)) break;
        he = hv_fetch_ent(hv, key, 0, 0);
        value = he ? HeVAL(he) : &PL_sv_undef;
        f->active.sv = value;
        if (SvROK(value)) last_n = 0;
        if (k) {
            bool newline = multiline && !last_n;
            dds_write(aTHX_ ctx, ctx->style.pairsep);
            ctx->column++;
            if (newline) { dds_put(aTHX_ ctx, "\n"); dds_spaces(aTHX_ ctx, indent); ctx->column = 0; }
            else {
                dds_write(aTHX_ ctx, ctx->style.space);
                if (!SvTRUE(ctx->style.space) && ctx->column > 1024) {
                    dds_put(aTHX_ ctx, "\n"); ctx->column = 0;
                }
            }
        }
        quoted = dds_quote(aTHX_ key, TRUE);
        dds_write(aTHX_ ctx, quoted);
        if (width) {
            UV len = sv_len_utf8(quoted);
            if (len <= width) dds_spaces(aTHX_ ctx, width - len);
            else { dds_put(aTHX_ ctx, "\n"); dds_spaces(aTHX_ ctx, indent + width); }
        }
        dds_write(aTHX_ ctx, ctx->style.space);
        dds_write(aTHX_ ctx, ctx->style.hashsep);
        dds_write(aTHX_ ctx, ctx->style.space);
        ctx->column += (width ? width : sv_len_utf8(quoted)) + 2*sv_len_utf8(ctx->style.space) + sv_len_utf8(ctx->style.hashsep);
        f->stage = DDS_STAGE_CHILD; f->keys = keys; f->k = k; f->last_n = last_n;
        f->width = width; f->indent = indent;
        dds_push_frame(aTHX_ ctx, DDS_FRAME_SV, value, depth + 1, mark,
                       dds_child_name(aTHX_ name, '{', key, 0), child_indent, FALSE);
        return FALSE;
      child_done:
        result = f->result; f->stage = DDS_STAGE_INITIAL;
        last_n = !full && !ctx->do_nl && ctx->column < 60 ? last_n + 1 : 0;
        if (result.alias) {
            SV *args = DDS_TEMP(newSVsv(dds_child_name(aTHX_ name, '%', NULL, 0)));
            sv_catpvs(args, ", "); DDS_CAT(args, dds_quote(aTHX_ key, FALSE));
            dds_call_fix(aTHX_ ctx, "alias_hv", args, result.alias);
        }
        f->active.sv = (SV *)hv;
    }
    dds_brace(aTHX_ ctx, name, '}', multiline, indent, count);
    Zero(&f->result, 1, dds_result);
    return TRUE;
}

static bool
dds_overloaded(pTHX_ SV *rv)
{
    return SvROK(rv) && SvOBJECT(SvRV(rv)) && SvAMAGIC(rv)
           && Gv_AMG(SvSTASH(SvRV(rv)));
}

static bool
dds_emit_ignored(pTHX_ dds_ctx *ctx, SV *rv, SV *target, bool forced)
{
    HV *style = dds_hash(aTHX_ ctx->object, "style", FALSE);
    HV *ignore = dds_hash(aTHX_ style, "ignore", FALSE);
    SV *key = DDS_TEMP(newSVpvf("#%" UVuf, PTR2UV(target)));
    SV *class_name = dds_class(aTHX_ target);
    bool ignored = forced || SvTRUE(dds_hget(aTHX_ ignore, DDS_PV(key)));
    if (SvOK(class_name)) {
        key = dds_concat(aTHX_ ".", class_name, "");
        ignored = ignored || SvTRUE(dds_hget(aTHX_ ignore, DDS_PV(key)));
    }
    if (ignored) {
        SV *text = DDS_TEMP(newSVpvs("Ignored Obj ["));
        SV *plain = DDS_TEMP(newSV(0));
        sv_setpvf(plain, "%s(0x%" UVxf ")", sv_reftype(target, TRUE), PTR2UV(target));
        if (SvOBJECT(target)) {
            sv_setpvf(plain, "%s=%s(0x%" UVxf ")", DDS_PV(class_name), sv_reftype(target, FALSE), PTR2UV(target));
        }
        DDS_CAT(text, plain); sv_catpvs(text, "]");
        dds_write(aTHX_ ctx, dds_quote(aTHX_ text, FALSE));
        PERL_UNUSED_ARG(rv);
    }
    return ignored;
}

static void
dds_emit_symbol(pTHX_ dds_ctx *ctx, GV *gv, SV *name, UV depth, bool deref)
{
    dds_node *node = dds_find(aTHX_ ctx, (SV *)gv, TRUE);
    if (deref) dds_put(aTHX_ ctx, "*{ ");
    if (!ctx->symbol_required) {
        dds_put(aTHX_ ctx, "do{ require Symbol; Symbol::gensym }");
        ctx->symbol_required = TRUE;
    }
    else dds_put(aTHX_ ctx, "Symbol::gensym");
    if (deref) dds_put(aTHX_ ctx, " }");
    if (ctx->style.dumpglob && !node->glob_dumped) {
        dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_GLOB, dds_child_name(aTHX_ name, '*', NULL, 0), NULL);
        node->glob_dumped = TRUE; fix->glob = (GV *)SvREFCNT_inc((SV *)gv); fix->depth = depth + 1;
        dds_add_fix(aTHX_ ctx, fix);
    }
}

static bool
dds_step_sv(pTHX_ dds_ctx *ctx, struct dds_frame *f)
{
    SV *sv = f->sv, *name = f->name;
    UV depth = f->depth;
    dds_mark *mark = f->mark;
    IV indent = f->indent;
    bool is_ref = f->is_ref;
    dds_result result = {NULL, NULL};
    dds_node *node = dds_candidate(aTHX_ sv) ? dds_find(aTHX_ ctx, sv, FALSE) : NULL;
    dds_role *role = node && node->slot.present ? &node->slot : NULL;
    SV *clean = SvOK(name) ? dds_clean_name(aTHX_ name) : &PL_sv_undef;
    bool readonly = role ? role->readonly : cBOOL(SvREADONLY(sv));
    bool add_do, is_ro;
    if (f->stage != DDS_STAGE_INITIAL) {
        role = f->role; add_do = f->add_do;
        goto child_done;
    }
    ctx->do_nl = FALSE;
    if (SvGMAGICAL(sv)) { dds_callback_guard(aTHX_ ctx); av_push(ctx->keepalive, SvREFCNT_inc(sv)); mg_get(sv); }
    if (role) {
        bool different = role->depth == depth && sv_cmp(role->name, clean)
                         && DDS_PV(name)[0] != '&';
        if (!role->original && (role->done || role->depth < depth || different)) {
            SV *text = DDS_TEMP(newSVpvs(""));
            if (is_ref) {
                if ((role->depth == 1 && !ctx->style.declare)
                    || (role->done && role->done->ready) || !ctx->style.purity) {
                    sv_catpvs(text, "\\"); DDS_CAT(text, role->name);
                }
                else {
                    bool need_do = strnEQ(DDS_PV(name), "$$$", 3);
                    dds_fix *fix;
                    if (need_do) { sv_catpvs(text, "do"); DDS_CAT(text, ctx->style.space); sv_catpvs(text, "{"); DDS_CAT(text, ctx->style.space); sv_catpvs(text, "my"); DDS_CAT(text, ctx->style.space); sv_catpvs(text, "$f"); DDS_CAT(text, ctx->style.space); sv_catpvs(text, "="); DDS_CAT(text, ctx->style.space); }
                    DDS_CAT(text, ctx->style.verbose ? dds_quote(aTHX_ dds_concat(aTHX_ "R: ", role->name, ""), FALSE) : DDS_TEMP(newSVpvs("'R'")));
                    result.deferred = dds_new_mark(aTHX_ ctx);
                    fix = dds_new_fix(aTHX_ DDS_FIX_SLOT, name, node);
                    fix->mark = result.deferred; dds_add_fix(aTHX_ ctx, fix);
                    if (need_do) { DDS_CAT(text, ctx->style.space); sv_catpvs(text, "}"); }
                }
            }
            else if (depth == 1) {
                if (ctx->style.declare) { sv_catpvs(text, "my "); DDS_CAT(text, name); sv_catpvs(text, ";\n"); }
                sv_catpvs(text, "alias_ref(\\"); DDS_CAT(text, name); sv_catpvs(text, ",\\"); DDS_CAT(text, role->name); sv_catpvs(text, ")");
            }
            else if (ctx->style.purity) {
                DDS_CAT(text, ctx->style.verbose ? dds_quote(aTHX_ dds_concat(aTHX_ "A: ", role->name, ""), FALSE) : DDS_TEMP(newSVpvs("'A'")));
                result.alias = node;
            }
            else { sv_catpvs(text, "alias_to("); DDS_CAT(text, role->name); sv_catpvs(text, ")"); }
            ctx->column += sv_len_utf8(text); dds_write(aTHX_ ctx, text);
            f->result = result;
            return TRUE;
        }
        if (!role->done) role->done = mark;
    }
    if (depth == 1) {
        if (role && sv_cmp(name, clean) && DDS_PV(name)[0] != '*' && role->count > 1) {
            if (node->ref.present) {
                SV *other = dds_child_name(aTHX_ node->ref.name, '$', NULL, 0);
                sv_setsv(role->name, other);
            }
            role->done = NULL;
        }
        if (!ctx->style.terse && DDS_PV(name)[0] != '&') {
            SV *prefix = DDS_TEMP(newSVpvs(""));
            if (ctx->style.declare && DDS_PV(name)[0] != '*'
                && !hv_exists_ent(ctx->lexical_added, name, 0)) {
                sv_catpvs(prefix, "my"); DDS_CAT(prefix, ctx->style.space);
            }
            DDS_CAT(prefix, name); DDS_CAT(prefix, ctx->style.space);
            sv_catpvs(prefix, "="); DDS_CAT(prefix, ctx->style.space);
            indent = sv_len_utf8(prefix); dds_write(aTHX_ ctx, prefix); ctx->column = 0;
        }
        else indent = 0;
    }
    if (is_ref) dds_put(aTHX_ ctx, "\\");
    add_do = ctx->style.purity && !readonly && is_ref
             && !(SvROK(sv) && SvOBJECT(SvRV(sv))) && SvTYPE(sv) != SVt_PVGV
             && (!SvROK(sv) || (SvTYPE(SvRV(sv)) <= SVt_PVMG && !SvROK(SvRV(sv)) && !SvREADONLY(SvRV(sv))));
    if (add_do) {
        dds_put(aTHX_ ctx, "do"); dds_write(aTHX_ ctx, ctx->style.space);
        dds_put(aTHX_ ctx, "{"); dds_write(aTHX_ ctx, ctx->style.space);
        dds_put(aTHX_ ctx, "my"); dds_write(aTHX_ ctx, ctx->style.space);
        dds_put(aTHX_ ctx, "$v"); dds_write(aTHX_ ctx, ctx->style.space);
        dds_put(aTHX_ ctx, "="); dds_write(aTHX_ ctx, ctx->style.space);
        ctx->column += 13;
    }
    is_ro = ctx->style.readonly && readonly && !is_ref;
    if (SvROK(sv)) {
        ctx->do_nl = TRUE;
        f->stage = DDS_STAGE_CHILD; f->role = role; f->add_do = add_do;
        dds_push_frame(aTHX_ ctx, DDS_FRAME_REF, sv, depth + 1, mark, name, indent, is_ref && !add_do);
        return FALSE;
    }
    else if (!SvOK(sv)) { dds_put(aTHX_ ctx, "undef"); ctx->column += 5; }
    else {
        if (is_ro && !ctx->style.purity && !ctx->style.terse) {
            dds_put(aTHX_ ctx, "make_ro("); dds_write(aTHX_ ctx, ctx->style.space);
        }
        if (SvTYPE(sv) == SVt_PVGV) {
            SV *glob = dds_glob_name(aTHX_ (GV *)sv);
            if (strnEQ(DDS_PV(glob), "*Symbol::GEN", 12))
                dds_emit_symbol(aTHX_ ctx, (GV *)sv, name, depth, TRUE);
            else {
                dds_node *gnode;
                dds_write(aTHX_ ctx, glob); ctx->column += sv_len_utf8(glob);
                gnode = ctx->style.dumpglob ? dds_find(aTHX_ ctx, sv, TRUE) : NULL;
                if (gnode && !gnode->glob_dumped) {
                    dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_GLOB, glob, NULL);
                    gnode->glob_dumped = TRUE; fix->glob = (GV *)SvREFCNT_inc(sv); fix->depth = depth + 1;
                    dds_add_fix(aTHX_ ctx, fix);
                }
            }
        }
        else {
            SV *quoted = NULL;
            if (ctx->style.dualvars && SvNIOK(sv) && SvPOK(sv)) {
                SV *number = DDS_TEMP(SvIOK(sv)
                    ? (SvIsUV(sv) ? newSVuv(SvUV(sv)) : newSViv(SvIV(sv)))
                    : newSVnv(SvNV(sv)));
                if (sv_cmp(number, sv)) {
                    quoted = DDS_TEMP(newSVpvs("dualvar( "));
                    DDS_CAT(quoted, number); sv_catpvs(quoted, ","); DDS_CAT(quoted, ctx->style.space);
                    DDS_CAT(quoted, dds_quote(aTHX_ sv, FALSE)); DDS_CAT(quoted, ctx->style.space); sv_catpvs(quoted, ")");
                }
            }
            if (!quoted && ctx->style.compress && SvTRUE(ctx->style.compressor)
                && (ctx->style.compress < 0 || sv_len_utf8(sv) > (UV)ctx->style.compress)) {
                SV *args[3];
                args[0] = sv; args[1] = ctx->owner;
                quoted = dds_invoke(aTHX_ ctx, ctx->style.compressor, FALSE, G_SCALAR, 2, args);
            }
            if (!quoted) quoted = dds_quote(aTHX_ sv, FALSE);
            ctx->column += sv_len_utf8(quoted); dds_write(aTHX_ ctx, quoted);
        }
        if (!ctx->style.terse) {
            if (is_ro && ctx->style.purity) dds_call_fix(aTHX_ ctx, "make_ro", name, NULL);
            else if (is_ro) { dds_write(aTHX_ ctx, ctx->style.space); dds_put(aTHX_ ctx, ")"); }
        }
        ctx->do_nl = FALSE;
    }
  child_done:
    if (add_do) { dds_write(aTHX_ ctx, ctx->style.space); dds_put(aTHX_ ctx, "}"); }
    if ((role && role->weak) || (SvROK(sv) && SvWEAKREF(sv)))
        dds_call_fix(aTHX_ ctx, "weaken", name, NULL);
    f->result = result;
    return TRUE;
}

static void
dds_queue_postdump(pTHX_ dds_ctx *ctx, dds_node *node, SV *original)
{
    dds_postop *postop;
    if (!node || node->ignored || node->postop_queued || !node->postop) return;
    /* Completion belongs to the original frozen object, not its proxy:
     * distinct originals can share one reconstructed replacement. Own a
     * snapshot from the already-fetched referent. Copying a magical RV
     * here would FETCH again and could select a different object. */
    Newxz(postop, 1, dds_postop);
    postop->node = node;
    node->postop_queued = TRUE;
    if (ctx->postop_tail) ctx->postop_tail->next = postop;
    else ctx->postop_head = postop;
    ctx->postop_tail = postop;
    postop->invocant = newRV_inc(original);
}

static void
dds_run_postdump(pTHX_ dds_ctx *ctx)
{
    /* A noncanonical occurrence may precede its proxy's construction.
     * Run restoration hooks only after all graph output and fixups are
     * complete, so hooks cannot change data still awaiting serialization. */
    while (ctx->postop_head) {
        dds_postop *postop = ctx->postop_head;
        dds_node *node = postop->node;
        SV *invocant = postop->invocant;
        ctx->postop_head = postop->next;
        if (!ctx->postop_head) ctx->postop_tail = NULL;
        Safefree(postop);
        DDS_TEMP(invocant);
        (void)dds_invoke(aTHX_ ctx, node->postop, !SvROK(node->postop),
                         G_VOID | G_DISCARD, 1, &invocant);
    }
}

static bool
dds_step_ref(pTHX_ dds_ctx *ctx, struct dds_frame *f)
{
    SV *rv = f->sv, *name = f->name;
    UV depth = f->depth;
    dds_mark *mark = f->mark;
    IV indent = f->indent;
    bool add_do = f->is_ref;
    SV *target, *class_name;
    dds_node *node;
    dds_role *role;
    bool overloaded, add_bless, add_lock, fix_lock = FALSE;
    AV *hidden = NULL;
    SV *thaw = NULL;
    bool inline_thaw = FALSE, function_thaw = FALSE;
    bool replacement = FALSE;
    UV proxy_limit = 0;
    if (f->stage != DDS_STAGE_INITIAL) {
        class_name = f->class_name;
        node = f->node; role = f->role;
        overloaded = f->overloaded; add_bless = f->add_bless;
        add_lock = f->add_lock; fix_lock = f->fix_lock; hidden = f->hidden;
        thaw = f->thaw;
        inline_thaw = f->inline_thaw; function_thaw = f->function_thaw;
        if (f->stage == DDS_STAGE_SCALAR_REF && f->result.deferred) role->done = f->result.deferred;
        goto child_done;
    }
    target = SvRV(rv);
    while (1) {
        bool force = replacement;
        if (dds_candidate(aTHX_ rv)) {
            dds_node *slot = dds_find(aTHX_ ctx, rv, FALSE);
            force = force || (slot && slot->slot.original);
        }
        node = force || dds_candidate(aTHX_ target) || SvOBJECT(target) || SvREADONLY(target)
               ? dds_find(aTHX_ ctx, target, FALSE) : NULL;
        if (node && node->ignored) {
            (void)dds_emit_ignored(aTHX_ ctx, rv, target, TRUE);
            return TRUE;
        }
        if (node && (!node->proxy || SvROK(node->proxy)))
            dds_queue_postdump(aTHX_ ctx, node, target);
        if (node && node->proxy) {
            if (!SvROK(node->proxy)) { dds_write(aTHX_ ctx, node->proxy); return TRUE; }
            thaw = node->thaw;
            rv = node->proxy; target = SvRV(rv);
            replacement = TRUE;
            if (thaw && SvTRUE(thaw) && !strnEQ(DDS_PV(thaw), "->", 2))
                mark = dds_new_mark(aTHX_ ctx);
            if (++proxy_limit > 100) croak("Cyclic freezer replacement");
            continue;
        }
        break;
    }
    if (dds_emit_ignored(aTHX_ ctx, rv, target, FALSE)) return TRUE;
    class_name = dds_class(aTHX_ target);
    if (SvTYPE(target) == SVt_REGEXP && SvOK(class_name) && strEQ(DDS_PV(class_name), "Regexp"))
        class_name = &PL_sv_undef;
    if (!node) {
        if (replacement || dds_candidate(aTHX_ target) || SvREADONLY(target) || SvTYPE(target) == SVt_PVGV) {
            node = dds_find(aTHX_ ctx, target, TRUE);
            if (!node->ref.present) {
                node->ref.present = TRUE; node->ref.name = newSVsv(name); node->ref.depth = depth;
            }
        }
        else {
            f->temporary.address = PTR2UV(target);
            f->temporary.ref.name = name; f->temporary.ref.depth = depth; node = &f->temporary;
        }
    }
    role = &node->ref;
    if (!role->name) { role->name = newSVsv(name); role->depth = depth; role->present = TRUE; }
    if (role->done && role->done->ready) {
        SV *text;
        if (strchr("@%&", DDS_PV(role->name)[0])) {
            if (SvTYPE(target) == SVt_PVHV && SvREADONLY(target)) {
                dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_LOCK, NULL, node);
                fix->args = (AV *)SvREFCNT_inc((SV *)dds_hidden_keys(aTHX_ (HV *)target));
                dds_add_fix(aTHX_ ctx, fix);
            }
            text = dds_ref_expression(aTHX_ ctx, role->name, class_name);
        }
        else text = role->name;
        dds_write(aTHX_ ctx, text); ctx->column += sv_len_utf8(text);
        return TRUE;
    }
    /* Glob-slot analysis and deferred emission have different frame
     * depths. The canonical expression, rather than that numeric depth,
     * decides which occurrence is allowed to construct the referent. */
    if (role->done || (sv_cmp(role->name, name)
            && DDS_PV(name)[0] != '&' && DDS_PV(role->name)[0] != '&')) {
        if (strchr("@%&", DDS_PV(role->name)[0]) && !ctx->style.declare)
        {
            SV *text = dds_ref_expression(aTHX_ ctx, role->name, class_name);
            dds_write(aTHX_ ctx, text); ctx->column += sv_len_utf8(text);
        }
        else if (!ctx->style.purity) {
            dds_write(aTHX_ ctx, role->name); ctx->column += sv_len_utf8(role->name);
        }
        else {
            dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_REF, name, node);
            SV *text = ctx->style.verbose
                ? dds_quote(aTHX_ dds_concat(aTHX_ "V: ", role->name, ""), FALSE)
                : DDS_TEMP(newSVpvs("'V'"));
            if (SvOK(class_name)) fix->class_name = newSVsv(class_name);
            if (add_do) { dds_put(aTHX_ ctx, "do { my $v = "); ctx->column += 12; }
            dds_write(aTHX_ ctx, text); ctx->column += sv_len_utf8(text);
            if (add_do) {
                dds_write(aTHX_ ctx, ctx->style.space); dds_put(aTHX_ ctx, "}");
                ctx->column += sv_len_utf8(ctx->style.space) + 1;
            }
            dds_add_fix(aTHX_ ctx, fix);
        }
        return TRUE;
    }
    role->done = mark;
    overloaded = dds_overloaded(aTHX_ rv);
    if (!thaw) thaw = node->thaw;
    if (thaw && SvTRUE(thaw)) {
        STRLEN len;
        const char *p = SvPV(thaw, len);
        bool simple = TRUE;
        STRLEN i;
        for (i = 0; i < len; i++) if (!isALNUMC_A(p[i]) && !strchr("_:>()-", p[i])) simple = FALSE;
        inline_thaw = simple && strnEQ(p, "->", 2);
        function_thaw = simple && len >= 2 && strEQ(p + len - 2, "()");
        if (inline_thaw && function_thaw) {
            dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(p + 2, len - 4)));
            dds_put(aTHX_ ctx, "("); dds_write(aTHX_ ctx, ctx->style.space);
        }
    }
    add_lock = SvTYPE(target) == SVt_PVHV && SvREADONLY(target);
    if (add_lock) {
        hidden = dds_hidden_keys(aTHX_ (HV *)target);
        if (DDS_PV(name)[0] != '$') { fix_lock = TRUE; add_lock = FALSE; }
        else { dds_put(aTHX_ ctx, av_len(hidden) >= 0 ? "lock_ref_keys_plus(" : "lock_ref_keys("); dds_write(aTHX_ ctx, ctx->style.space); }
    }
    add_bless = SvOK(class_name) && !strchr("@%&", DDS_PV(name)[0]);
    if (add_bless && !overloaded) {
        STRLEN len; const char *p = SvPV(ctx->style.bless, len);
        dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(p, len ? len - 1 : 0)));
        dds_write(aTHX_ ctx, ctx->style.space);
    }
    f->class_name = SvREFCNT_inc(class_name);
    f->node = node; f->role = role;
    f->overloaded = overloaded; f->add_bless = add_bless;
    f->add_lock = add_lock; f->fix_lock = fix_lock;
    f->hidden = (AV *)SvREFCNT_inc((SV *)hidden);
    f->thaw = thaw;
    f->inline_thaw = inline_thaw; f->function_thaw = function_thaw;
    ctx->do_nl = TRUE;
    if (SvTYPE(target) == SVt_REGEXP)
        dds_write(aTHX_ ctx, dds_regex(aTHX_ rv));
    else if (SvTYPE(target) == SVt_PVAV || SvTYPE(target) == SVt_PVHV) {
        f->stage = DDS_STAGE_CHILD;
        dds_push_frame(aTHX_ ctx, SvTYPE(target) == SVt_PVAV ? DDS_FRAME_ARRAY : DDS_FRAME_HASH,
                       target, depth, mark, name, indent, FALSE);
        ctx->frames->node = node == &f->temporary ? NULL : node;
        return FALSE;
    }
    else if (SvTYPE(target) == SVt_PVCV || SvTYPE(target) == SVt_PVFM) {
        SV *args[4], *code;
        args[0] = rv; args[1] = name; args[2] = DDS_TEMP(newSViv(indent)); args[3] = class_name;
        code = dds_call(aTHX_ ctx, SvTYPE(target) == SVt_PVCV ? "_xs_code_text" : "_xs_format_text",
                        ctx->owner, G_SCALAR, 4, args);
        dds_write(aTHX_ ctx, code);
    }
    else if (SvTYPE(target) == SVt_PVIO)
        dds_put(aTHX_ ctx, "*{Symbol::gensym()}{IO}");
    else if (SvTYPE(target) == SVt_PVGV && strnEQ(DDS_PV(dds_glob_name(aTHX_ (GV *)target)), "*Symbol::GEN", 12))
        dds_emit_symbol(aTHX_ ctx, (GV *)target, name, depth, FALSE);
    else if (SvTYPE(target) <= SVt_PVMG || SvTYPE(target) == SVt_PVGV) {
        f->stage = DDS_STAGE_SCALAR_REF;
        dds_push_frame(aTHX_ ctx, DDS_FRAME_SV, target, depth + 1, mark,
                       dds_child_name(aTHX_ name, '$', NULL, 0), indent, TRUE);
        return FALSE;
    }
    else croak("Cannot emit Perl SV type %d", (int)SvTYPE(target));
  child_done:
    if (add_bless) {
        if (!overloaded) {
            STRLEN len; const char *p = SvPV(ctx->style.bless, len);
            dds_put(aTHX_ ctx, ","); dds_write(aTHX_ ctx, ctx->style.space);
            dds_write(aTHX_ ctx, dds_quote(aTHX_ class_name, FALSE)); dds_write(aTHX_ ctx, ctx->style.space);
            if (len) dds_write(aTHX_ ctx, DDS_TEMP(newSVpvn(p + len - 1, 1)));
        }
        else {
            dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_BLESS, NULL, node);
            fix->text = newSVsv(class_name); dds_add_fix(aTHX_ ctx, fix);
        }
    }
    if (fix_lock && !SvOK(class_name)) {
        dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_LOCK, NULL, node);
        fix->args = (AV *)SvREFCNT_inc((SV *)hidden); dds_add_fix(aTHX_ ctx, fix);
    }
    if (add_lock) {
        SSize_t i;
        for (i = 0; i <= av_len(hidden); i++) {
            dds_put(aTHX_ ctx, ","); dds_write(aTHX_ ctx, ctx->style.space);
            dds_write(aTHX_ ctx, dds_quote(aTHX_ *av_fetch(hidden, i, 0), FALSE));
        }
        dds_write(aTHX_ ctx, ctx->style.space); dds_put(aTHX_ ctx, ")");
    }
    if (thaw && SvTRUE(thaw)) {
        if (inline_thaw) {
            if (function_thaw) { dds_write(aTHX_ ctx, ctx->style.space); dds_put(aTHX_ ctx, ")"); }
            else { dds_put(aTHX_ ctx, "->"); dds_put(aTHX_ ctx, DDS_PV(thaw) + 2); dds_put(aTHX_ ctx, "()"); }
        }
        else {
            dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_THAW, NULL, node);
            fix->text = newSVsv(thaw); dds_add_fix(aTHX_ ctx, fix);
        }
    }
    ctx->do_nl = TRUE;
    Zero(&f->result, 1, dds_result);
    return TRUE;
}

dds_result
dds_emit_sv(pTHX_ dds_ctx *ctx, SV *sv, UV depth, dds_mark *mark,
            SV *name, IV indent, bool is_ref)
{
    struct dds_frame *base = ctx->frames;
    dds_result result = {NULL, NULL};
    dds_push_frame(aTHX_ ctx, DDS_FRAME_SV, sv, depth, mark, name, indent, is_ref);
    while (ctx->frames != base) {
        struct dds_frame *frame = ctx->frames;
        bool done = FALSE;
        ENTER;
        SAVETMPS;
        switch (frame->kind) {
            case DDS_FRAME_SV: done = dds_step_sv(aTHX_ ctx, frame); break;
            case DDS_FRAME_REF: done = dds_step_ref(aTHX_ ctx, frame); break;
            case DDS_FRAME_ARRAY: done = dds_step_array(aTHX_ ctx, frame); break;
            case DDS_FRAME_HASH: done = dds_step_hash(aTHX_ ctx, frame); break;
        }
        if (done) {
            result = frame->result;
            ctx->frames = frame->parent;
            ctx->active = frame->active.parent;
            if (ctx->frames != base) ctx->frames->result = result;
            frame->parent = ctx->free_frames; ctx->free_frames = frame;
            dds_release_frame(aTHX_ frame);
        }
        FREETMPS;
        LEAVE;
    }
    return result;
}

void
dds_emit_glob(pTHX_ dds_ctx *ctx, GV *gv, SV *name, UV depth)
{
    UV i, limit = ctx->style.deparse && ctx->style.deparseglob ? 4 : 3;
    static const char *slots[] = {"SCALAR", "HASH", "ARRAY", "CODE"};
    for (i = 0; i < limit; i++) {
        SV *value, *target, *rv;
        dds_node *node;
        dds_mark *mark;
        dds_result result;
        SV *path;
        UV replacements = 0;
        /* Fetch each slot from the owned glob immediately before use;
         * earlier slot callbacks may have replaced later slots. */
        switch (i) {
            case 0: value = GvSV(gv); break;
            case 1: value = (SV *)GvHV(gv); break;
            case 2: value = (SV *)GvAV(gv); break;
            default: value = (SV *)GvCV(gv); break;
        }
        if (!value || (i == 0 && !SvOK(value))) continue;
        target = value;
        node = dds_find(aTHX_ ctx, target, FALSE);
        if (node && (!node->proxy || SvROK(node->proxy)) && node->postop && !node->postop_queued)
            dds_queue_postdump(aTHX_ ctx, node, target);
        while (node && node->proxy && SvROK(node->proxy)) {
            target = SvRV(node->proxy);
            node = dds_find(aTHX_ ctx, target, FALSE);
            if (node && (!node->proxy || SvROK(node->proxy)) && node->postop && !node->postop_queued)
                dds_queue_postdump(aTHX_ ctx, node, target);
            if (++replacements > 100) croak("Cyclic freezer replacement");
        }
        if (!node || !node->ref.present) continue;
        path = DDS_TEMP(newSVsv(name));
        sv_catpvf(path, "{%s}", slots[i]);
        if (sv_cmp(node->ref.name, path)) {
            dds_fix *fix = dds_new_fix(aTHX_ DDS_FIX_REF, name, node);
            SV *class_name = dds_class(aTHX_ target);
            if (SvOK(class_name)) fix->class_name = newSVsv(class_name);
            dds_add_fix(aTHX_ ctx, fix);
            continue;
        }
        rv = DDS_TEMP(newRV_inc(value));
        mark = dds_new_mark(aTHX_ ctx);
        if (!ctx->style.terse) { dds_write(aTHX_ ctx, name); dds_put(aTHX_ ctx, " = "); }
        /* Only the actual canonical slot may construct the value. Other
         * slots defer their assignment until that construction is ready. */
        result = dds_emit_sv(aTHX_ ctx, rv, depth, mark, path, sv_len_utf8(name) + 3, FALSE);
        if (result.alias) croak("Unexpected alias for glob slot");
        dds_put(aTHX_ ctx, ";\n"); mark->ready = TRUE;
    }
    if (ctx->style.deparse && ctx->style.deparseglob && GvFORM(gv)) {
        SV *args[2], *text;
        args[0] = DDS_TEMP(newRV_inc((SV *)gv)); args[1] = name;
        text = dds_call(aTHX_ ctx, "_xs_glob_format_text", ctx->owner, G_SCALAR, 2, args);
        if (SvOK(text)) dds_write(aTHX_ ctx, text);
    }
}

SV *
dds_output(pTHX_ dds_ctx *ctx, I32 gimme)
{
    UV i;
    AV *names, *declared;
    SV *printer;
    dds_clear_emission(aTHX_ ctx);
    dds_load_style(aTHX_ ctx);
    for (i = 0; ctx->cataloged && i < ctx->root_count; i++)
        if (ctx->roots[i].analyzed_ref != (SvROK(ctx->roots[i].sv) ? SvRV(ctx->roots[i].sv) : NULL))
            ctx->cataloged = FALSE;
    if (!ctx->cataloged) dds_analyze(aTHX_ ctx);
    ctx->out = newSVpvs("");
    if (gimme == G_ARRAY) ctx->chunks = newAV();
    printer = dds_hget(aTHX_ ctx->object, "fh");
    if (SvTRUE(printer)) ctx->printer = newSVsv(printer);
    names = newAV(); declared = newAV();
    hv_stores(ctx->object, "out_names", newRV_noinc((SV *)names));
    hv_stores(ctx->object, "declare", newRV_noinc((SV *)declared));
    for (i = 0; i < ctx->root_count; i++) {
        av_push(names, newSVsv(ctx->roots[i].name));
        av_push(declared, newSVsv(ctx->roots[i].name));
    }
    if (ctx->style.compress && SvTRUE(ctx->style.compressor)) {
        SV *prelude = dds_invoke(aTHX_ ctx, ctx->style.compressor, FALSE, G_SCALAR, 0, NULL);
        if (SvTRUE(prelude)) dds_write(aTHX_ ctx, prelude);
    }
    if (HvTOTALKEYS(ctx->lexical_added)) {
        AV *keys = (AV *)DDS_TEMP((SV *)newAV());
        HE *he; SSize_t k;
        hv_iterinit(ctx->lexical_added);
        while ((he = hv_iternext(ctx->lexical_added))) av_push(keys, newSVsv(hv_iterkeysv(he)));
        sortsv(AvARRAY(keys), av_len(keys) + 1, Perl_sv_cmp);
        dds_put(aTHX_ ctx, "my (");
        for (k = 0; k <= av_len(keys); k++) {
            if (k) dds_put(aTHX_ ctx, ",");
            dds_write(aTHX_ ctx, *av_fetch(keys, k, 0));
        }
        dds_put(aTHX_ ctx, ");\n");
    }
    for (i = 0; i < ctx->root_count; i++) {
        dds_mark *mark = dds_new_mark(aTHX_ ctx);
        dds_result result = dds_emit_sv(aTHX_ ctx, ctx->roots[i].sv, 1, mark, ctx->roots[i].name, 0, FALSE);
        if (result.alias) croak("Unexpected root alias");
        dds_put(aTHX_ ctx, ";\n"); mark->ready = TRUE;
        dds_apply_fixes(aTHX_ ctx, FALSE);
    }
    dds_apply_fixes(aTHX_ ctx, TRUE);
    ctx->final_weakening = TRUE;
    dds_apply_fixes(aTHX_ ctx, TRUE);
    if (ctx->fix_head) croak("DDS cannot resolve graph fixups; call Data again if the graph changed");
    dds_run_postdump(aTHX_ ctx);
    if (gimme == G_VOID && !ctx->printer) {
        SV *args[2];
        args[0] = DDS_TEMP(newRV_inc((SV *)gv_fetchpvs("main::STDOUT", GV_ADD, SVt_PVIO)));
        args[1] = ctx->out;
        /* Dispatch through the actual glob: it may be localized or tied.
         * PRINT is user code and needs the same guard as a To callback. */
        (void)dds_invoke(aTHX_ ctx, DDS_TEMP(newSVpvs("print")), TRUE,
                         G_VOID | G_DISCARD, 2, args);
    }
    return ctx->out;
}
