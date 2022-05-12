from os import path, popen

qtile_path = path.join(path.expanduser('~'), ".config", "qtile")
scripts_path = popen('echo $SCRIPTS').read().replace('\n', '')
# scripts_path = path.join(path.expanduser('~'), ".local", "bin")
