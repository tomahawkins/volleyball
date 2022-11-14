{
  setting-lib = haskell.library {
    public = [
      ./Cache.hs
    ];
    deps = [ "//hackage:SHA" "//hackage:tagsoup" ];
    visibility = visibility.public;
  };

  setting-run = haskell.executable {
    main = ./SettingRow.hs;
    deps = [ "//hackage:tagsoup" ":setting-lib" ];
  };
}
