let
  luks-swap-uuid = "dc619c76-9689-4b2a-8a42-8566831b2500";
in
{
  boot.initrd = {
    # setup keyfile for a luks-encrypted hard drive
    secrets = {
      "/crypto_keyfile.bin" = null;
    };

    # enable swap on a luks-encrypted hard drive
    luks.devices."luks-${luks-swap-uuid}" = {
      device = "/dev/disk/by-uuid/${luks-swap-uuid}";
      keyFile = "/crypto_keyfile.bin";
    };
  };
}
