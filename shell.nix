with (import ./default.nix {});
app.env.overrideAttrs (drv: {
  shellHook = ''
     export PATH=$PATH:${pkgs.gnused}/bin/sed
  '';
})
