# emacs-zulip

An Appkit-based, multi-account Zulip client for Emacs, with a navigator and
chat-buffer experience inspired by telega.el, disco.el, and emacs-qq.

The package currently provides a complete text-message vertical slice:

- one Appkit application per normalized `(server, email)` account;
- non-secret configured account targets with API keys resolved through
  `auth-source`;
- `/register`, consecutive long-poll event processing, retry, and expired-queue
  re-registration;
- normalized users, subscriptions, unread state, recent direct conversations,
  messages, and account-scoped server topic metadata;
- an account navigator for all messages, subscribed channels, topics, and
  recent direct conversations;
- combined, channel, topic, exact-participant direct-message, mention,
  starred-message, and server-search narrows;
- exact history windows, first-unread loading, older/newer paging, and opening
  a message with surrounding context;
- a telega-style timeline with compact consecutive messages, date and unread
  dividers, progressively cached sender avatars, channel/topic breadcrumbs,
  reaction chips, and an inline star;
- observable auto-read, explicit read/unread actions, starring, reactions,
  editing, and confirmed deletion;
- an Appkit composer with Markdown, real Org parsing, plain-text mode,
  Telega-style prefix selection, pure preview, structured mentions, formatting
  edits, optimistic sends, stable rekeying, and retryable failed rows; and
- an optional clickable global mode-line unread/mention indicator.

Zulip message IDs are opaque decimal strings at state and Surface boundaries.
They are never converted to an Emacs number for identity or ordering.  A send
temporarily owns a `local-*` row key; the row is promoted and rekeyed to the
authoritative server ID when either the event or HTTP response arrives.  When
an endpoint such as `/messages/flags` requires JSON integers, the HTTP adapter
validates those strings and emits their digits directly as numeric JSON tokens;
it never round-trips an ID through an Emacs number.

## Requirements

emacs-zulip requires Emacs 32.0 and Appkit 0.3.0.  Markdown composition also
requires the `markdown` and `markdown-inline` Tree-sitter grammars.  Check or
install Appkit's pinned grammar versions explicitly:

```elisp
(appkit-markup-markdown-ts-ready-p)
(appkit-markup-markdown-ts-install-grammars)
```

The installer is never invoked by account startup, preview, or send.  Markdown
parsing uses Tree-sitter directly without enabling `markdown-ts-mode`, Font
Lock, embedded code language modes, or user hooks.

## Ownership and lifecycle

Each normalized account is the canonical `appkit-app-model`: a
`zulip-account` containing protocol state accessed through
`zulip-account-state`, pending sends, topic metadata, and decoded avatar data.
Creating an account does not perform network I/O.  Connecting explicitly starts
an App-owned Source for registration, consecutive polling, retry, and queue
replacement.  Source results carry the exact account, App, and generation;
stopping or reconnecting revokes the old transport and retry capability.

The root navigator and each narrow are Generated Surfaces with stable
account-scoped identities, retrieved through `appkit-app-surface`.  App domain
transitions post messages to those Surfaces; updates commit projection changes
without rendering performing I/O.  Surface HTTP Effects own history, message
actions, and editing requests.  History operations fence the current request
and bind its returned HTTP handle to the real Surface owner, so cancellation
and late responses cannot cross a replacement Surface or history generation.
Optimistic sends are App-owned and can settle after their originating feed
closes.

Stopping preserves the host buffer and its editable draft while revoking the
old Surface's authority.  Reopening reuses only an unattached, package-owned
host with the same account and narrow identity; it preserves the derived major
mode, structured draft, and active compose codec.  An account
whose event Source has not been enabled is offline, not connecting.

Topic hydration uses a bounded FIFO task queue owned by the root Surface;
repeated refreshes deduplicate active and queued channels, and detached
schedulers cannot land results or start waiting work.  Avatars are declarative
Resources requested by dependent rows, with account-scoped decoded images and
Appkit-managed acquisition and cancellation.

## Account configuration

Configure only account metadata in Emacs:

```elisp
(setq zulip-accounts
      '((:name "work"
         :server "https://chat.example.com"
         :email "person@example.com")
        (:name "community"
         :server "https://chat.example.org:8443"
         :email "person@example.org")))
```

Each server must be an HTTPS origin without credentials, path, query, or
fragment.  Names and normalized `(server, email)` identities must be unique.
With one target, `M-x zulip` uses it directly; with several, completion shows
only the local name, email, and server.  With no configured target, Zulip
prompts for a server and email.

API keys come from `auth-source`.  For the accounts above, an encrypted
`~/.authinfo.gpg` can contain:

```text
machine chat.example.com login person@example.com port zulip password API_KEY
machine chat.example.org login person@example.org port zulip-8443 password API_KEY
```

Default HTTPS uses the service name `zulip`; a non-default origin port uses
`zulip-PORT`.  The same exact host, email, and service tuple is used for
lookup, so unrelated web credentials cannot satisfy a Zulip account.
Standard auth-source backends such as encrypted authinfo and password-store
remain available through `auth-sources`.

The selected API key is copied into the live account only after lookup.  The
account erases its owned copy when credentials are replaced or the Appkit
session stops.  API keys never enter Customize data, completion labels,
buffer names, or status messages.

## Navigator

`M-x zulip` opens the account root immediately while registration continues in
the background.  Registration and event updates refresh that buffer in place,
preserving its identity and selected row.  Server topic metadata is hydrated
for subscriptions, so topics need not already have a locally cached message to
appear.  `g` retries topic metadata and retains the last good cache if a
request fails.  To keep large realms responsive, the root normally shows at
most `zulip-root-visible-topics-per-channel` useful/recent topics per channel;
unread and mentioned topics are always retained.  `t` and `/` still discover
the complete topic cache.  Set the option to nil to render every topic row.

Navigator keys:

| Key | Action |
| --- | --- |
| `RET`, mouse-1 | Open the destination at point |
| `n`, `p` | Move to the next or previous destination |
| `u` | Move to the next destination with unread messages |
| `/` | Complete any displayed destination or a discovery action |
| `t` | Choose a channel and open an existing or new topic |
| `s` | Search messages on the server |
| `m` | Start a direct message by choosing participants |
| `g` | Refresh the projection and server topic metadata |
| `q` | Quit the window |

The Messages section exposes all messages, mentions, and starred messages.
Channels contain their known topics; recent direct conversations follow them.
`M-x zulip-open-combined-feed` remains available for opening the all-messages
feed directly.

## Feeds and message actions

All feed variants share one Appkit timeline and exact-history controller.
Initial loading anchors around `first_unread` when canonical unread state says
the narrow contains unread messages; otherwise it loads the newest page.
Approaching an edge pages automatically by default.  The explicit history
commands are:

| Key | Action |
| --- | --- |
| `M-g p` | Load one older page |
| `M-g n` | Load one newer page of a partial window |
| `C-c C-l` | Reload the newest page |

Outside the composer, point-local single-key commands operate on the message
at point:

| Key | Action |
| --- | --- |
| `n`, `p` | Move to the next or previous message |
| `RET`, `o` | Open the message's topic or direct conversation |
| `t` | Open a topic, defaulting from the message at point |
| `c` | Copy the message as plain text |
| `r`, `u` | Mark read through point, or mark the message unread |
| `s` | Toggle starred state |
| `!` | Add or remove a reaction |
| `e` | Edit the message's raw Markdown in the composer |
| `d` | Delete after confirmation |
| `R` | Retry a failed optimistic send with the same `local-*` identity |
| `?` | Open the discoverable message-action transient |
| `q` | Quit the window |

Reaction chips and failed-send rows are also clickable.  Auto-read follows
messages actually observed at point or at the visible end of the selected
window.  It only submits unread, server-backed messages in the current
contiguous history window; it does not guess across gaps.  Set
`zulip-auto-mark-read` to nil to disable it.  Explicitly marking a message
unread suppresses immediate automatic reversal until it is deliberately
observed again.

## Composer

A trailing composer appears only for a topic or an exact-participant direct
message, because those narrows identify an unambiguous send destination.
Combined, channel, mention, starred, and search feeds keep all timeline
navigation and message actions but have no composer.

| Key | Action |
| --- | --- |
| `RET` | Send with the active codec, or complete an `@mention`/`:emoji:` |
| `C-u RET` | Insert a newline |
| `TAB`, `C-M-i` | Complete an `@mention` or Unicode `:emoji:` |
| `C-c RET`, `C-c C-c` | Send with the active codec, or submit an edit |
| `C-u C-c C-c` | Parse as the second configured codec for this send |
| `C-u C-u C-c C-c` | Parse as the third configured codec for this send |
| `C-c C-v` | Open a non-interactive semantic preview; prefixes select codecs |
| `C-c C-m` | Select the persistent active source codec |
| `M-p`, `M-n` | Browse composer input history |
| `C-c '` | Move point to the composer |
| `C-c C-k` | Cancel an edit and restore its draft and active codec |
| `C-c C-a` | Open the message-action transient |

`zulip-compose-codecs` defaults to `(markdown org plain)`.  Selection follows
Telega's universal-prefix ordering.  The composer leaves source text under user
control and does not inject formatting delimiters.  Markdown and Org source are
parsed into one immutable Appkit Document; the same capture drives preview,
optimistic semantic display, and canonical Zulip-compatible Markdown output.
Markdown uses Appkit's pinned block and inline Tree-sitter adapters.  Org uses
Emacs's built-in `org-element` without enabling Org mode, Babel, Font Lock, or
user hooks.  Unsupported conversion, such as Org underline to Markdown, is
rejected instead of silently flattened.

Mention completion inserts a human-readable, atomic `@Name` object.  At output
time its semantic object prints Zulip's stable ID-qualified mention syntax.
Entering an edit switches its server-supplied raw source to Markdown and
snapshots the current rich draft; cancelling or completing restores both that
draft and its prior active codec.

Sends first insert an optimistic `local-*` row.  An event and its HTTP response
may arrive in either order; promotion is idempotent.  A failed row remains in
the timeline and can be clicked or retried with `R` without changing its local
identity.

## Evil

When Evil is installed, `zulip-evil-enable-integration` enables native normal-
and motion-state bindings without raising the ordinary Zulip maps above Evil.
Native operators and prefixes such as `gg` and `e` therefore keep their Evil
meaning.  Application commands use deliberate aliases such as `g r` for
refresh, uppercase message actions, and `i` to focus the composer and enter
insert state in one command.  Set `zulip-evil-initial-state` to nil to retain
your own initial-state policy.

## Global mode line

Enable `M-x zulip-mode-line-mode` to aggregate unread and mention counts across
live accounts.  The Zulip label opens an account root, while the unread and
mention counts are clickable shortcuts into the account navigator.

## Current limitations

The Appkit 0.3.0 lifecycle, semantic markup, native UI, codec registry,
source-backed compose capture, Generated Surfaces, sectioned directory,
timeline, history, chat buffer, completion, responsive layout, mode-line, and
avatar infrastructure are integrated.  Zulip's authoritative server-rendered HTML is
parsed with libxml into Appkit Documents; message buffers do not use SHR.
User/group mentions, channel/topic/message links, timestamps, spoilers, emoji,
media, and unsupported provider structures remain typed Zulip objects with safe
visible fallbacks.  Links and Zulip navigation become native Appkit actions.

Sender avatars use declarative Appkit Resources and its disk cache: rows retain
stable initials geometry while loading, credentials are sent only to same-origin
realm URLs, and Resource results refresh only sender-dependent rows.
Remaining rich-media limitations:

- inline images and embeds use safe link placeholders rather than in-buffer
  image acquisition;
- spoiler content remains visible until per-Surface reveal state is implemented;
- tables and unsupported provider blocks use semantic text fallbacks;
- uploads and attachment previews are not implemented; and
- realm/custom emoji catalogs and image-backed reaction rendering are not
  implemented.  Completion currently covers Unicode emoji only.

These are client adapter gaps, not claims about what Appkit itself can do.

## Development

This package is developed beside `appkit.el`.  Eask installs the package source
and dependencies on its load path.  ERT fixtures use isolated real account Apps
and Generated Surfaces with synthetic transports; shared runtime fixtures
deliver only their own queued App and Surface work.  They do not resolve user
credentials or connect to a Zulip server.

Install the local Appkit build, recompile, and run all tests with:

```sh
eask run script test-local
```

For an already prepared sandbox, run only the tests with:

```sh
eask test ert test/*.el
```
