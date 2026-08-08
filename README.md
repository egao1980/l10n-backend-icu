# l10n-backend-icu

[`l10n-protocol`](https://github.com/egao1980/l10n-protocol) backend over **ICU4C** via [`cl-stack-icu`](https://github.com/egao1980/cl-stack-icu).

## Capabilities

`:collate` `:number` `:date` `:currency` `:list` `:locale-case`

Relative time via `format-relative-time` (ICU `RelativeDateTimeFormatter`).

```lisp
(asdf:load-system "l10n-backend-icu")
(collate "a" "b" :locale "en")
(format-number 1234.5 :locale "en_US")
(format-relative-time -1 :day :locale "en" :numeric :always)
```

## License

MIT
