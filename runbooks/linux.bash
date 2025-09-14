systemctl --user daemon-reload; \
    systemctl --user enable emacs.service

journalctl --user -xeu emacs.service

systemctl --user status emacs.service

mu init --my-address jeremy@jeremyfriesen.com
