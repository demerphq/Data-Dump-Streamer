#ifndef DDS_ENGINE_H
#define DDS_ENGINE_H

#define PERL_NO_GET_CONTEXT
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

typedef struct dds_node dds_node;
typedef struct dds_mark dds_mark;
typedef struct dds_ctx dds_ctx;
typedef struct dds_active {
    struct dds_active *parent;
    SV *sv;
    SV *retained_sv, *retained_ref;
} dds_active;

typedef struct {
    /* Catalogue roles own path strings; frame-local roles borrow their
     * frame's name. done borrows an emission mark. */
    SV *name;
    SV *original;
    UV depth, count, encounters;
    dds_mark *done;
    bool present, readonly, weak;
} dds_role;

struct dds_node {
    dds_node *next;
    /* Opaque identity key. Never convert this address back into an SV. */
    UV address;
    dds_role slot, ref;
    /* Owned freezer results. These are independent of the address key. */
    SV *proxy, *thaw, *postop;
    UV key_width, key_count;
    bool frozen, expanded, ignored, glob_dumped, postop_queued;
};

typedef struct {
    dds_node **buckets;
    UV mask, count;
} dds_table;

typedef struct {
    SV *sv, *name;
    SV *analyzed_ref;
    UV count;
    bool readonly, lexical;
} dds_root;

struct dds_mark {
    dds_mark *next;
    /* The corresponding construction is available for dependent fixups.
     * A deferred scalar reference can substitute its own completion mark. */
    bool ready;
};

typedef enum {
    DDS_FIX_SLOT, DDS_FIX_REF, DDS_FIX_CALL, DDS_FIX_BLESS,
    DDS_FIX_LOCK, DDS_FIX_THAW, DDS_FIX_GLOB, DDS_FIX_VAR
} dds_fix_kind;

typedef struct dds_fix {
    struct dds_fix *next;
    dds_fix_kind kind;
    /* Owned output strings/arguments. node and mark borrow stable C
     * records that outlive the fixup until emission/catalogue cleanup. */
    SV *lhs, *text, *class_name;
    AV *args;
    dds_node *node;
    dds_mark *mark;
    GV *glob; /* Owned only for deferred glob emission. */
    UV depth;
} dds_fix;

typedef struct dds_postop {
    struct dds_postop *next;
    dds_node *node; /* Borrowed catalogue record, never an input SV address. */
    SV *invocant;  /* Owned snapshot of the original object's RV. */
} dds_postop;

typedef struct {
    SV *sv, *name;
    UV depth, next;
    bool direct;
} dds_queue_entry;

typedef struct {
    UV head, tail; /* Queue indices plus one; zero denotes an empty bucket. */
} dds_queue_level;

typedef struct {
    SV *space, *hashsep, *arysep, *pairsep, *bless;
    SV *compressor, *codestub, *formatstub;
    IV indent, indentcols, compress;
    bool indentkeys, declare, verbose, dumpglob, deparse, deparseglob;
    bool rle, readonly, dualvars, purity, terse, special;
} dds_style;

struct dds_ctx {
    HV *object;
    SV *owner;
    dds_root *roots;
    UV root_count, root_capacity;
    dds_table table;
    dds_queue_entry *queue;
    UV queue_head, queue_count, queue_capacity;
    dds_queue_level *queue_levels;
    UV queue_depth, queue_level_capacity;
    dds_mark *marks;
    dds_fix *fix_head, *fix_tail, *glob_head;
    dds_postop *postop_head, *postop_tail;
    HV *types, *abbreviations, *lexical_names, *lexical_added;
    AV *keepalive;
    AV *style_values;
    dds_active *active;
    struct dds_frame *frames, *free_frames;
    SV *out;
    AV *chunks;
    SV *printer;
    dds_style style;
    UV visits, lookups, candidates, repeats, queue_peak, fixes;
    IV column;
    bool cataloged, busy, do_nl, symbol_required, callbacks, final_weakening;
};

typedef struct {
    dds_node *alias;
    dds_mark *deferred;
} dds_result;

SV *dds_hget(pTHX_ HV *hv, const char *key);
HV *dds_hash(pTHX_ HV *hv, const char *key, bool create);
AV *dds_array(pTHX_ HV *hv, const char *key, bool create);
SV *dds_call(pTHX_ dds_ctx *ctx, const char *sub, SV *invocant, I32 flags,
             I32 argc, SV **argv);
SV *dds_invoke(pTHX_ dds_ctx *ctx, SV *callable, bool method, I32 flags,
               I32 argc, SV **argv);
SV *dds_freeze(pTHX_ dds_ctx *ctx, SV *rv);
SV *dds_new_object(pTHX_ SV *class_name);
SV *dds_safe_self(pTHX_ SV *self);
SV *dds_working_self(pTHX_ SV *self);
void dds_api_boot(pTHX);
const char *dds_builtin_order(pTHX_ SV *mode);
void dds_callback_guard(pTHX_ dds_ctx *ctx);
void *dds_operation_begin(pTHX_ dds_ctx *ctx, SV *self);
void dds_operation_commit(void *operation);
dds_ctx *dds_context(pTHX_ SV *self);
void dds_capture(pTHX_ dds_ctx *ctx, I32 count, SV **args);
void dds_clear_catalogue(pTHX_ dds_ctx *ctx);
void dds_clear_emission(pTHX_ dds_ctx *ctx);
void dds_clear_frames(pTHX_ dds_ctx *ctx);
void dds_load_style(pTHX_ dds_ctx *ctx);
void dds_analyze(pTHX_ dds_ctx *ctx);
SV *dds_output(pTHX_ dds_ctx *ctx, I32 gimme);
dds_node *dds_find(pTHX_ dds_ctx *ctx, SV *sv, bool create);
UV dds_weak_count(pTHX_ SV *sv);
bool dds_candidate(pTHX_ SV *sv);
SV *dds_class(pTHX_ SV *sv);
SV *dds_quote(pTHX_ SV *value, bool key);
SV *dds_qquote(pTHX_ SV *value);
SV *dds_name(pTHX_ dds_ctx *ctx, SV *sv, UV root_index);
SV *dds_child_name(pTHX_ SV *parent, char kind, SV *key, IV child_index);
SV *dds_clean_name(pTHX_ SV *name);
SV *dds_underef(pTHX_ SV *name);
SV *dds_glob_name(pTHX_ GV *gv);
AV *dds_keys(pTHX_ dds_ctx *ctx, HV *hv, I32 pass, SV **thaw);
AV *dds_hidden_keys(pTHX_ HV *hv);
SV *dds_regex(pTHX_ SV *rv);
void dds_write(pTHX_ dds_ctx *ctx, SV *text);
void dds_put(pTHX_ dds_ctx *ctx, const char *text);
void dds_spaces(pTHX_ dds_ctx *ctx, IV count);
void dds_add_fix(pTHX_ dds_ctx *ctx, dds_fix *fix);
dds_fix *dds_new_fix(pTHX_ dds_fix_kind kind, SV *lhs, dds_node *node);
dds_mark *dds_new_mark(pTHX_ dds_ctx *ctx);
void dds_apply_fixes(pTHX_ dds_ctx *ctx, bool final);
dds_result dds_emit_sv(pTHX_ dds_ctx *ctx, SV *sv, UV depth,
                       dds_mark *mark, SV *name, IV indent, bool is_ref);
void dds_emit_glob(pTHX_ dds_ctx *ctx, GV *gv, SV *name, UV depth);

#define DDS_TEMP(sv) sv_2mortal(sv)
#define DDS_PV(sv) SvPV_nolen(sv)
#define DDS_CAT(dst, sv) sv_catsv((dst), (sv))
#define DDS_TRUE(ctx, key) SvTRUE(dds_hget(aTHX_ (ctx)->object, (key)))

#endif
