{ config, pkgs, ... }:
let unstable =
    import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/0747387223edf1aa5beaedf48983471315d95e16.tar.gz)
    # reuse the current configuration
    { config = config.nixpkgs.config; };

    unstablePrime =
    import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/294ef54a1e8cdcdd298c79edbdb3713ceae46988.tar.gz)
    # reuse the current configuration
    { config = config.nixpkgs.config; };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # <home-manager/nixos>
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  networking.hostName = "paxos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Istanbul";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp0s31f6.useDHCP = true;
  networking.interfaces.wlp1s0.useDHCP = true;
  networking.networkmanager.enable = true;
  nixpkgs.config.allowUnfree = true;

  nix.useSandbox = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the Plasma 5 Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  # Enable XMonad 
  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
	haskellPackages.xmonad
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
	];
      };
    };

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable bluetooth and get blueman-applet/blueman-manager
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dad = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "docker"]; # Enable ‘sudo’ for the user.
  };

  # home-manager.users.dad = { pkgs, ... }: {
  #   home.packages = [ pkgs.git ];
  # };  

  # nix.settings.experimental-features = [ "nix-command" "flakes" ];
  #
  # nix.extraOptions = ''
  #   keep-outputs = true
  #   keep-derivations = true
  # '';
  # 
  # environment.pathsToLink = [
  #   "/share/nix-direnv"
  # ];
  #
  # nixpkgs.overlays = [
  #   (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = true; }; } )
  # ];
  
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    git
    alacritty
    chromium curl unstable.emacs
    dolphin
    okular

    nixos-icons
    breeze-icons
    libsForQt5.breeze-icons
    kdeFrameworks.breeze-icons
    kdeFrameworks.oxygen-icons5

    kdeApplications.kio-extras
    kdeApplications.kdegraphics-thumbnailers

    dmenu networkmanager networkmanager_dmenu
    unstable.firefox-devedition-bin feh
    gimp gwenview
    # haskellPackages.ghc
    unstable.haskellPackages.ghc
    unstable.haskellPackages.zlib
    # broken in both 20.09 and unstable
    # haskellPackages.libpq
    unstablePrime.tdesktop
    haskellPackages.postgresql-libpq
    haskellPackages.ghcid
    haskellPackages.cabal-install
    htop i3lock plasma-pa
    haskellPackages.status-notifier-item
    unstablePrime.spotify scrot wget zlib
    python39
    unstable.texlive.combined.scheme-full
    font-awesome
    iosevka
    polybar
    rofi
    unzip
    gnumake
    openssl
    jq
    irssi
    guvcview
    #docker
    #docker-compose
    pavucontrol
    mpv
    gcc
    neofetch
    tree
    ripgrep
    fd
    unstable.zoom-us
    direnv
    nix-direnv
    xorg.xmodmap
  ];

  programs.light.enable = true;

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_12;
    };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  virtualisation.docker.enable = true;
  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?


}

