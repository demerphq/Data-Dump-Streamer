#define NEED_mg_findext
#include "dds_engine.h"

SV *
dds_hget(pTHX_ HV *hv, const char *key)
{
    SV **svp = hv ? hv_fetch(hv, key, (I32)strlen(key), 0) : NULL;
    return svp ? *svp : &PL_sv_undef;
}

SV *
dds_hget_sv(pTHX_ HV *hv, SV *key)
{
    HE *entry = hv ? hv_fetch_ent(hv, key, 0, 0) : NULL;
    return entry ? HeVAL(entry) : &PL_sv_undef;
}

HV *
dds_hash(pTHX_ HV *hv, const char *key, bool create)
{
    SV *sv = dds_hget(aTHX_ hv, key);
    HV *result;
    if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVHV)
        return (HV *)SvRV(sv);
    if (!create)
        return NULL;
    result = newHV();
    hv_store(hv, key, (I32)strlen(key), newRV_noinc((SV *)result), 0);
    return result;
}

AV *
dds_array(pTHX_ HV *hv, const char *key, bool create)
{
    SV *sv = dds_hget(aTHX_ hv, key);
    AV *result;
    if (SvROK(sv) && SvTYPE(SvRV(sv)) == SVt_PVAV)
        return (AV *)SvRV(sv);
    if (!create)
        return NULL;
    result = newAV();
    hv_store(hv, key, (I32)strlen(key), newRV_noinc((SV *)result), 0);
    return result;
}

/* Pointer hashing follows Sereal's ptable.h, derived from the Perl pointer
 * table and Chocolateboy's autobox implementation, under Perl's license. */
static UV
dds_pointer_hash(UV address)
{
    UV x = address;
    x ^= x >> 4;
    x *= (UV)2654435761UL;
    x ^= x >> 16;
    return x;
}

dds_node *
dds_find(pTHX_ dds_ctx *ctx, SV *sv, bool create)
{
    dds_table *table = &ctx->table;
    UV bucket;
    UV address = PTR2UV(sv);
    dds_node *node;
    ctx->lookups++;
    if (!table->buckets) {
        if (!create)
            return NULL;
        table->mask = 31;
        Newxz(table->buckets, table->mask + 1, dds_node *);
    }
    bucket = dds_pointer_hash(address) & table->mask;
    for (node = table->buckets[bucket]; node; node = node->next)
        if (node->address == address)
            return node;
    if (!create)
        return NULL;
    Newxz(node, 1, dds_node);
    node->address = address;
    node->next = table->buckets[bucket];
    table->buckets[bucket] = node;
    table->count++;
    ctx->candidates++;
    if (table->count > (table->mask + 1) * 2) {
        UV i, mask = table->mask * 2 + 1;
        dds_node **buckets;
        Newxz(buckets, mask + 1, dds_node *);
        for (i = 0; i <= table->mask; i++) {
            dds_node *entry = table->buckets[i];
            while (entry) {
                dds_node *next = entry->next;
                bucket = dds_pointer_hash(entry->address) & mask;
                entry->next = buckets[bucket];
                buckets[bucket] = entry;
                entry = next;
            }
        }
        Safefree(table->buckets);
        table->buckets = buckets;
        table->mask = mask;
    }
    return node;
}

static SV *
dds_backrefs(pTHX_ SV *sv)
{
#if PERL_VERSION >= 22
    return sv_get_backrefs(sv);
#else
    MAGIC *mg;
# if PERL_VERSION >= 10
    if (SvTYPE(sv) == SVt_PVHV && SvOOK(sv))
        return (SV *)HvAUX((HV *)sv)->xhv_backreferences;
# endif
    mg = SvMAGICAL(sv) ? mg_find(sv, PERL_MAGIC_backref) : NULL;
    return mg ? mg->mg_obj : NULL;
#endif
}

UV
dds_weak_count(pTHX_ SV *sv)
{
    SV *refs = dds_backrefs(aTHX_ sv);
    UV count = 0;
    if (!refs)
        return 0;
    if (SvTYPE(refs) == SVt_PVAV) {
        SSize_t i;
        for (i = 0; i <= av_len((AV *)refs); i++) {
            SV **entry = av_fetch((AV *)refs, i, 0);
            if (entry && SvROK(*entry) && SvWEAKREF(*entry)
                && SvRV(*entry) == sv)
                count++;
        }
    }
    else if (SvROK(refs) && SvWEAKREF(refs) && SvRV(refs) == sv)
        count = 1;
    return count;
}

bool
dds_candidate(pTHX_ SV *sv)
{
    if (!sv || sv == &PL_sv_undef || sv == &PL_sv_yes || sv == &PL_sv_no)
        return FALSE;
    return SvREFCNT(sv) > 1 || dds_backrefs(aTHX_ sv) != NULL;
}

SV *
dds_class(pTHX_ SV *sv)
{
    SV *name;
    if (!SvOBJECT(sv) || !SvSTASH(sv))
        return &PL_sv_undef;
    name = DDS_TEMP(newSVpv(HvNAME_get(SvSTASH(sv)), 0));
    if (HvNAMEUTF8(SvSTASH(sv))) SvUTF8_on(name);
    return name;
}

void
dds_clear_emission(pTHX_ dds_ctx *ctx)
{
    dds_mark *mark;
    dds_fix *fix;
    dds_postop *postop;
    UV i;
    dds_clear_frames(aTHX_ ctx);
    while ((postop = ctx->postop_head)) {
        SV *invocant = postop->invocant;
        ctx->postop_head = postop->next;
        if (!ctx->postop_head) ctx->postop_tail = NULL;
        Safefree(postop);
        SvREFCNT_dec(invocant);
    }
    for (i = 0; i < 2; i++) {
        dds_fix **head = i ? &ctx->glob_head : &ctx->fix_head;
        while ((fix = *head)) {
            *head = fix->next;
            SvREFCNT_dec(fix->lhs);
            SvREFCNT_dec(fix->text);
            SvREFCNT_dec(fix->class_name);
            SvREFCNT_dec(fix->args);
            SvREFCNT_dec(fix->glob);
            SvREFCNT_dec(fix->path);
            Safefree(fix);
        }
    }
    ctx->fix_tail = NULL;
    ctx->pending_aliases = 0;
    while ((mark = ctx->marks)) {
        ctx->marks = mark->next;
        Safefree(mark);
    }
    if (ctx->table.buckets)
        for (i = 0; i <= ctx->table.mask; i++) {
            dds_node *node;
            for (node = ctx->table.buckets[i]; node; node = node->next) {
                node->slot.done = node->ref.done = NULL;
                node->glob_dumped = FALSE;
                node->postop_queued = FALSE;
                node->class_restored = FALSE;
            }
        }
    SvREFCNT_dec(ctx->out);
    SvREFCNT_dec(ctx->chunks);
    SvREFCNT_dec(ctx->printer);
    ctx->out = ctx->printer = NULL;
    ctx->chunks = NULL;
    ctx->column = 0;
    ctx->symbol_required = FALSE;
    ctx->final_weakening = FALSE;
}

void
dds_clear_catalogue(pTHX_ dds_ctx *ctx)
{
    UV i;
    dds_clear_emission(aTHX_ ctx);
    if (ctx->table.buckets)
        for (i = 0; i <= ctx->table.mask; i++) {
            dds_node *node = ctx->table.buckets[i];
            while (node) {
                dds_node *next = node->next;
                SvREFCNT_dec(node->slot.name);
                SvREFCNT_dec(node->slot.original);
                SvREFCNT_dec(node->ref.name);
                SvREFCNT_dec(node->ref.original);
                SvREFCNT_dec(node->proxy);
                SvREFCNT_dec(node->thaw);
                SvREFCNT_dec(node->postop);
                SvREFCNT_dec(node->class_fields);
                SvREFCNT_dec(node->class_fields_name);
                SvREFCNT_dec(node->class_package);
                Safefree(node);
                node = next;
            }
            ctx->table.buckets[i] = NULL;
        }
    ctx->table.count = 0;
    ctx->class_head = ctx->class_tail = NULL;
    ctx->class_serial = 0;
    for (i = 0; i < ctx->queue_count; i++)
        SvREFCNT_dec(ctx->queue[i].name);
    ctx->queue_head = ctx->queue_count = 0;
    if (ctx->queue_levels)
        Zero(ctx->queue_levels, ctx->queue_level_capacity, dds_queue_level);
    ctx->queue_depth = 0;
    hv_clear(ctx->types);
    hv_clear(ctx->abbreviations);
    hv_clear(ctx->lexical_names);
    hv_clear(ctx->lexical_added);
    av_clear(ctx->keepalive);
    ctx->callbacks = ctx->cataloged = FALSE;
    ctx->active = NULL;
    ctx->visits = ctx->lookups = ctx->candidates = ctx->repeats = 0;
    ctx->queue_peak = ctx->fixes = 0;
}

static int
dds_magic_free(pTHX_ SV *sv, MAGIC *mg)
{
    dds_ctx *ctx = (dds_ctx *)mg->mg_ptr;
    UV i;
    PERL_UNUSED_ARG(sv);
    if (!ctx)
        return 0;
    mg->mg_ptr = NULL;
    dds_clear_catalogue(aTHX_ ctx);
    for (i = 0; i < ctx->root_count; i++) {
        SvREFCNT_dec(ctx->roots[i].sv);
        SvREFCNT_dec(ctx->roots[i].name);
    }
    Safefree(ctx->roots);
    Safefree(ctx->queue);
    Safefree(ctx->queue_levels);
    Safefree(ctx->table.buckets);
    SvREFCNT_dec(ctx->types);
    SvREFCNT_dec(ctx->abbreviations);
    SvREFCNT_dec(ctx->lexical_names);
    SvREFCNT_dec(ctx->lexical_added);
    SvREFCNT_dec(ctx->keepalive);
    SvREFCNT_dec(ctx->style_values);
    Safefree(ctx);
    return 0;
}

static MGVTBL dds_magic_vtbl = {0, 0, 0, 0, dds_magic_free, 0, 0, 0};

dds_ctx *
dds_context(pTHX_ SV *self)
{
    HV *hv;
    MAGIC *mg;
    dds_ctx *ctx;
    if (!SvROK(self) || SvTYPE(SvRV(self)) != SVt_PVHV)
        croak("Data::Dump::Streamer requires an object");
    hv = (HV *)SvRV(self);
    mg = mg_findext((SV *)hv, PERL_MAGIC_ext, &dds_magic_vtbl);
    if (mg)
        return (dds_ctx *)mg->mg_ptr;
    Newxz(ctx, 1, dds_ctx);
    ctx->object = hv;
    ctx->types = newHV();
    ctx->abbreviations = newHV();
    ctx->lexical_names = newHV();
    ctx->lexical_added = newHV();
    ctx->keepalive = newAV();
    ctx->style_values = newAV();
    sv_magicext((SV *)hv, NULL, PERL_MAGIC_ext, &dds_magic_vtbl,
                (const char *)ctx, 0);
    return ctx;
}

void
dds_capture(pTHX_ dds_ctx *ctx, I32 count, SV **args)
{
    I32 i;
    dds_root *roots, *old_roots = ctx->roots;
    UV old_count = ctx->root_count;
    Newxz(roots, count ? count : 1, dds_root);
    for (i = 0; i < count; i++) {
        roots[i].sv = SvREFCNT_inc(args[i]);
        roots[i].count = SvREFCNT(args[i]) + dds_weak_count(aTHX_ args[i]);
        roots[i].readonly = cBOOL(SvREADONLY(args[i]));
    }
    /* Publish retained input before releasing values whose destructors may
     * recursively use the dumper. They must never see dying old roots. */
    ctx->roots = roots;
    ctx->root_count = ctx->root_capacity = count;
    dds_clear_catalogue(aTHX_ ctx);
    for (i = 0; i < (I32)old_count; i++) {
        SvREFCNT_dec(old_roots[i].sv);
        SvREFCNT_dec(old_roots[i].name);
    }
    Safefree(old_roots);
}

void
dds_load_style(pTHX_ dds_ctx *ctx)
{
    HV *style = dds_hash(aTHX_ ctx->object, "style", FALSE);
    dds_style *s = &ctx->style;
    av_clear(ctx->style_values);
#define STR(field, key) STMT_START { s->field = newSVsv(dds_hget(aTHX_ style, key)); av_push(ctx->style_values, s->field); } STMT_END
#define NUM(field, key) s->field = SvIV(dds_hget(aTHX_ style, key))
#define BOOL(field, key) s->field = cBOOL(SvTRUE(dds_hget(aTHX_ style, key)))
    STR(space, "optspace"); STR(hashsep, "hashsep"); STR(arysep, "arysep");
    STR(pairsep, "pairsep"); STR(bless, "bless"); STR(compressor, "compressor");
    STR(codestub, "codestub"); STR(formatstub, "formatstub");
    NUM(indent, "indent"); NUM(indentcols, "indentcols"); NUM(compress, "compress");
    BOOL(indentkeys, "indentkeys"); BOOL(declare, "declare"); BOOL(verbose, "verbose");
    BOOL(dumpglob, "dumpglob"); BOOL(deparse, "deparse"); BOOL(deparseglob, "deparseglob");
    BOOL(rle, "rle"); BOOL(readonly, "ro"); BOOL(dualvars, "dualvars");
    BOOL(purity, "purity"); BOOL(terse, "terse"); BOOL(special, "special");
#undef STR
#undef NUM
#undef BOOL
}

/* Called only when Perl code can run. Retain current work, never values
 * recovered from catalogue addresses. Ordinary graph walks borrow SVs. */
void
dds_callback_guard(pTHX_ dds_ctx *ctx)
{
    UV i;
    dds_active *active;
    if (!ctx->callbacks) {
        ctx->callbacks = TRUE;
        for (i = 0; i < ctx->root_count; i++)
            av_push(ctx->keepalive, SvREFCNT_inc(ctx->roots[i].sv));
        if (ctx->queue_head < ctx->queue_count)
            for (i = 0; i < ctx->queue_count; i++)
                if (ctx->queue[i].name)
                    av_push(ctx->keepalive, SvREFCNT_inc(ctx->queue[i].sv));
    }
    for (active = ctx->active; active; active = active->parent) {
        SV *referent = SvROK(active->sv) ? SvRV(active->sv) : NULL;
        /* Ancestor frames recur at every deeper callback. Keep their
         * unchanged SVs once, but still retain replacements after mutation. */
        if (active->retained_sv != active->sv) {
            av_push(ctx->keepalive, SvREFCNT_inc(active->sv));
            active->retained_sv = active->sv;
        }
        if (referent && active->retained_ref != referent) {
            av_push(ctx->keepalive, SvREFCNT_inc(referent));
            active->retained_ref = referent;
        }
    }
}

typedef struct {
    dds_ctx *ctx;
    bool committed;
} dds_operation;

static void
dds_operation_cleanup(pTHX_ void *ptr)
{
    dds_operation *operation = (dds_operation *)ptr;
    dds_ctx *ctx = operation->ctx;
    ctx->active = NULL;
    ctx->owner = NULL;
    ctx->busy = FALSE;
    if (!operation->committed) {
        dds_clear_catalogue(aTHX_ ctx);
        hv_stores(ctx->object, "cataloged", newSViv(0));
    }
    Safefree(operation);
}

void *
dds_operation_begin(pTHX_ dds_ctx *ctx, SV *self)
{
    dds_operation *operation;
    if (ctx->busy) croak("DDS operation already active");
    SAVEFREESV(SvREFCNT_inc(self));
    Newxz(operation, 1, dds_operation);
    operation->ctx = ctx;
    ctx->busy = TRUE;
    ctx->owner = self;
    SAVEDESTRUCTOR_X(dds_operation_cleanup, operation);
    return operation;
}

void dds_operation_commit(void *operation)
{ ((dds_operation *)operation)->committed = TRUE; }

SV *
dds_invoke(pTHX_ dds_ctx *ctx, SV *callable, bool method, I32 flags,
           I32 argc, SV **argv)
{
    dSP;
    I32 i, count;
    SV *result = DDS_TEMP(newSV(0));
    AV *values = NULL;
    AV *arguments = (AV *)DDS_TEMP((SV *)newAV());
    SV *error = (flags & G_EVAL) ? DDS_TEMP(newSV(0)) : NULL;
    /* argv may point into the Perl stack. Retain its SVs before EXTEND
     * can relocate it; do not retain addresses of argument slots. */
    for (i = 0; i < argc; i++) av_push(arguments, SvREFCNT_inc(argv[i]));
    callable = DDS_TEMP(newSVsv(callable));
    if (ctx) dds_callback_guard(aTHX_ ctx);
    ENTER;
    SAVETMPS;
    PUSHMARK(SP);
    EXTEND(SP, argc);
    for (i = 0; i < argc; i++)
        PUSHs(AvARRAY(arguments)[i]);
    PUTBACK;
    count = method ? call_method(DDS_PV(callable), flags) : call_sv(callable, flags);
    SPAGAIN;
    if (error) sv_setsv(error, ERRSV);
    if ((flags & G_WANT) == G_ARRAY) {
        values = newAV();
        sv_setsv(result, DDS_TEMP(newRV_noinc((SV *)values)));
        for (i = count; i > 0; i--)
            av_push(values, newSVsv(SP[1 - i]));
        SP -= count;
    }
    else {
        SV *value = count ? POPs : &PL_sv_undef;
        sv_setsv(result, value);
        if (count > 1)
            SP -= count - 1;
    }
    PUTBACK;
    FREETMPS;
    LEAVE;
    if (error) sv_setsv(ERRSV, error);
    return result;
}

SV *
dds_call(pTHX_ dds_ctx *ctx, const char *sub, SV *invocant, I32 flags,
         I32 argc, SV **argv)
{
    SV *callable = DDS_TEMP(newSVpv(sub, 0));
    if (invocant) {
        AV *args = (AV *)DDS_TEMP((SV *)newAV());
        I32 i;
        av_push(args, SvREFCNT_inc(invocant));
        for (i = 0; i < argc; i++) av_push(args, SvREFCNT_inc(argv[i]));
        return dds_invoke(aTHX_ ctx, callable, TRUE, flags, argc + 1, AvARRAY(args));
    }
    return dds_invoke(aTHX_ ctx, callable, FALSE, flags, argc, argv);
}

SV *
dds_freeze(pTHX_ dds_ctx *ctx, SV *rv)
{
    SV *target = SvRV(rv), *class_name = dds_class(aTHX_ target);
    HV *style = dds_hash(aTHX_ ctx->object, "style", FALSE);
    HV *classes = dds_hash(aTHX_ style, "freeze_class", FALSE);
    HV *skip = dds_hash(aTHX_ ctx->object, "cache_skip_freeze", TRUE);
    HV *cache = dds_hash(aTHX_ ctx->object, "cache_freeze_class", TRUE);
    SV *cached = dds_hget_sv(aTHX_ cache, class_name);
    SV *choices[3];
    UV i;
    if (SvTRUE(dds_hget_sv(aTHX_ skip, class_name))) return &PL_sv_undef;
    if (SvOK(cached)) {
        SV *values = dds_invoke(aTHX_ ctx, cached, !SvROK(cached), G_ARRAY, 1, &rv);
        AV *list = (AV *)SvRV(values);
        SV **proxy = av_fetch(list, 0, 0);
        /* Only the first undef disables a class hook. Once selected, an
         * empty or undef result means ignore this object, like false. */
        if (!proxy || !SvOK(*proxy)) av_store(list, 0, newSViv(0));
        return values;
    }
    choices[0] = DDS_TEMP(newSVsv(dds_hget_sv(aTHX_ classes, class_name)));
    choices[1] = DDS_TEMP(newSVsv(dds_hget(aTHX_ style, "freezer")));
    choices[2] = DDS_TEMP(newSVpvs("DDS_freeze"));
    for (i = 0; i < 3; i++) {
        SV *action = choices[i], *values;
        SV **proxy;
        if (!SvTRUE(action)) { if (SvOK(action)) break; else continue; }
        if (!SvROK(action) && !gv_fetchmethod_autoload(SvSTASH(target), DDS_PV(action), 0)) continue;
        values = dds_invoke(aTHX_ ctx, action, !SvROK(action), G_ARRAY | G_EVAL, 1, &rv);
        if (SvROK(ERRSV) || SvTRUE(ERRSV)) { sv_setpvs(ERRSV, ""); continue; }
        proxy = av_fetch((AV *)SvRV(values), 0, 0);
        if (proxy && SvOK(*proxy)) hv_store_ent(cache, class_name, newSVsv(action), 0);
        else hv_store_ent(skip, class_name, newSViv(1), 0);
        return values;
    }
    hv_store_ent(skip, class_name, newSViv(1), 0);
    return &PL_sv_undef;
}
