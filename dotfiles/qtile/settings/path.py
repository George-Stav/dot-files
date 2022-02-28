from os import path, popen

qtile_path = path.join(path.expanduser('~'), ".config", "qtile")
scripts_path = popen('echo $SCRIPTS').read().replace('\n', '')
