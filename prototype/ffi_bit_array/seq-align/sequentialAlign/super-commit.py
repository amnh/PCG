#! /usr/bin/env python3

''' super-commit.py solves a problem: a nested git directory (the child) needs to be committed to
    two git repos.

    Takes a single argv, a string, which is the commit message. Checks which directory it's run from,
    commits in that directory and also the other (could be parent or child). Directory paths are
    relative, in that the complete directory path is machine-independent, but absolute in that the
    relation between the parent and child directories is hard-coded.
'''

import os
import subprocess
import sys

if len(sys.argv) != 2:
    print("\nNeed one and only one argument, a commit message.\n\n")
    exit()

seqAlignDir = "seq-align/sequentialAlign"
prodDir     = "phylocomgraph/production"

curDir = '/'.join(os.getcwd().split('/')[-2:])

if curDir == prodDir:
    otherDirPath  = '/'.join(os.getcwd().split('/')[:-1]) + '/prototype/ffi_bit_array/' + seqAlignDir
    out, err = subprocess.Popen(['git', 'branch', '|', 'grep', "'*'"], stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()

elif curDir == seqAlignDir:
    otherDirPath = '/'.join(os.getcwd().split('/')[:-5]) + '/' + prodDir
    # can't check for master in seqAlignDir, because it's automatically master
    out, err = subprocess.Popen(['git', 'branch', '|', 'grep', "'*'"], cwd=otherDirPath, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()

else:
    print('\nNot in either production or sequential align directory.\n')
    exit()

print(str(out, 'utf-8'))
if str(out, 'utf-8').split()[1] != 'master':
    print("\nCan't commit because not in master branch of production.\n")

exit()

commit_string  = sys.argv[1]
commit_command = ['git', 'commit', '-a', '-m', '"' + commit_string + '"']
log_command    = ['git', 'log']

# for current directory only need subprocess.call()
local_commit = subprocess.call(commit_command)


# for other directory I'm changing working directory, so need Popen() and wait()
other_commit = subprocess.Popen(commit_command, cwd=otherDirPath)
other_commit.wait()


print('\n\nSuccess!\n', flush=True)