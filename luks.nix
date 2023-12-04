let luks-swap-uuid = "578b5840-2be0-4eab-a2e8-1748cfdf7da0";
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
