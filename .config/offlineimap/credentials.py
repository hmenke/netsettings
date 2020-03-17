#!/usr/bin/env python

import netrc
from os.path import expanduser, isfile
import sys

filename = expanduser("~/.config/offlineimap/netrc")

credentials = netrc.netrc(filename)

def get_username(host):
    login, _, _ = credentials.hosts[host]
    return login

def get_password(host):
    _, _, password = credentials.hosts[host]
    return password

if __name__ == "__main__":
    host = sys.argv[1]
    login = get_username(host)
    password = get_password(host)
    print("set imap_user = {}".format(login))
    print("set imap_pass = {}".format(password))

    # Decide whether to use the offline or online configuration
    if isfile(expanduser("~/.config/mutt/mbnames")):
        print('set folder = ~/.local/share/offlineimap/GMail')
        print('source "~/.config/mutt/mbnames"')
    else:
        print('set folder = "imaps://{}@imap.gmail.com/"'.format(login))
