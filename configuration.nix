# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:
# Pinned at August 8, 2021
# https://github.com/NixOS/nixpkgs/commit/439b1605227b8adb1357b55ce8529d541abbe9eb
let unstable =
    import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/439b1605227b8adb1357b55ce8529d541abbe9eb.tar.gz) { config = config.nixpkgs.config; };
    dadFork = import ./../../home/dad/src/nixpkgs { };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;    

  networking.hostName = "yakushima"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Istanbul";

  nixpkgs.config.allowUnfree = true;
  
  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp3s0.useDHCP = true;

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
  #services.xserver.displayManager.sddm.enable = true;
  #services.xserver.desktopManager.plasma5.enable = true;

  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.epkowa ];  
  
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
  hardware.pulseaudio.extraModules = [ pkgs.pulseaudio-modules-bt ];

  # Enable bluetooth    
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  
  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.dad = {
    isNormalUser = true;
    extraGroups = [ "wheel" "video" "scanner" "lp"]; # Enable ‘sudo’ for the user.
  };

  fonts.fonts = with pkgs; [
    font-awesome
    iosevka
    (nerdfonts.override { fonts = [ "Iosevka" ]; })
  ];

  fonts.fontconfig.defaultFonts = {
    monospace = [
      "Iosevka"
    ];
    sansSerif = [
      "Iosevka"
    ];
    serif = [
      "Iosevka"
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    git
    alacritty
    chromium curl emacs
    dolphin
    okular

    nixos-icons
    
    breeze-icons
    oxygen-icons5    

    libsForQt5.breeze-icons
    libsForQt5.oxygen-icons5

    libsForQt5.kio-extras
    libsForQt5.kdegraphics-thumbnailers    
    

    dmenu networkmanager networkmanager_dmenu
    unstable.firefox-devedition-bin feh
    git gimp gwenview
    haskellPackages.ghc
    haskellPackages.zlib
    tdesktop
    haskellPackages.postgresql-libpq
    haskellPackages.ghcid
    haskellPackages.cabal-install
    htop i3lock plasma-pa
    haskellPackages.status-notifier-item
    spotify scrot wget zlib
    python39
    polybar
    rofi
    unzip
    gnumake
    openssl
    jq
    irssi
    guvcview
    pavucontrol
    mpv
    gcc
    neofetch
    tree
    ripgrep
    fd
    pciutils
    killall
    cabal2nix
    nixfmt
    cargo
    rustc
    xclip
    unstable.texlive.combined.scheme-full
    xdotool
    haskellPackages.termonad
  ];

  # gamer dad
  programs.steam.enable = true;
  
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

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?

}

