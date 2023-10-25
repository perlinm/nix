{
  boot.initrd = {
    # setup keyfile for encrypted hard drive
    secrets = { "/crypto_keyfile.bin" = null; };

    # enable swap on encrypted disks
    luks.devices."luks-6584249e-94c5-4559-a9e8-3654b2b164ae" = {
      device = "/dev/disk/by-uuid/6584249e-94c5-4559-a9e8-3654b2b164ae";
      keyFile = "/crypto_keyfile.bin";
    };
  };
}
