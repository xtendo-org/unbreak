# run before release.
strip --strip-all --remove-section=.comment --remove-section=.note ~/.local/bin/unbreak && upx -9 ~/.local/bin/unbreak
