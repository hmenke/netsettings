#!/usr/bin/env python

import netrc
import os
import sys

filename = os.path.expanduser("~/.config/offlineimap/netrc")

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
