#!/usr/bin/env python

def main():
    import os
    os.system("scalac *.scala")
    return 0

if __name__ == "__main__":
    import sys
    sys.exit(main())
