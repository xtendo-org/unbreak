# run before release.
strip --strip-all --remove-section=.comment --remove-section=.note ~/.local/bin/u && upx -9 ~/.local/bin/u
