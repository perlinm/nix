let luks-swap-uuid = "6584249e-94c5-4559-a9e8-3654b2b164ae";
in {
  boot.initrd = {
    # setup keyfile for a luks-encrypted hard drive
    secrets = { "/crypto_keyfile.bin" = null; };

    # enable swap on a luks-encrypted hard drive
    luks.devices."luks-${luks-swap-uuid}" = {
      device = "/dev/disk/by-uuid/${luks-swap-uuid}";
      keyFile = "/crypto_keyfile.bin";
    };
  };
}
