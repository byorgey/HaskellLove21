#!/usr/bin/env python3

import time
import glob
from subprocess import call, run
import subprocess
import sys

class bcolors:
    GREEN = '\033[92m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    OK = GREEN + BOLD
    FAIL = RED + BOLD
    ENDC = '\033[0m'

solution_file = ''
if len(sys.argv) > 1:
    solution_file = sys.argv[1]
else:
    for code_file in glob.glob('*.hs'):
        solution_file = code_file

cabal_file = ''
for f in glob.glob('*.cabal'):
    cabal_file = f

sol_file_components = solution_file.split('.')
sol_name = sol_file_components[0]

print(f'Using {solution_file}')

print(f'Compiling {solution_file}...')
if cabal_file != '':
    result = run(['cabal', 'build'])
else:
    result = run(['ghc', '--make', '-O2', '-package', 'mtl', '-package', 'split', '-package', 'parsec', '-package', 'vector', '-package', 'unordered-containers', '-package', 'hashable', solution_file])

if result.returncode != 0:
    sys.exit(1);

ok = True

for test_input in sorted(glob.glob('*.in')):
    print(test_input + ": ", end='')
    test_name = '.'.join(test_input.split('.')[0:-1])
    test_output = test_name + ".out"
    test_answer = test_name + ".ans"
    print("running... ", end='')
    start = time.time()
    if cabal_file != '':
        run(f"cabal -v0 run {sol_name} < {test_input} > {test_output}", shell=True)
    else:
        run(f"./{sol_name} < {test_input} > {test_output}", shell=True)
    end = time.time()
    print("checking output... ", end='')
    result = run(["diff", '-b', test_answer, test_output], stdout=subprocess.PIPE)
    if result.returncode == 0:
        print(bcolors.OK + "OK" + bcolors.ENDC, end=' ')
        print(f'({(end-start):.2}s)')
    else:
        print(bcolors.FAIL + "Fail" + bcolors.ENDC, end=' ')
        print(f'({(end-start):.2}s)')
        print(result.stdout.decode('utf-8'))
        ok = False

print("Cleaning up...")
if cabal_file == '':
    run(f"rm {sol_name} *.o *.hi", shell=True)
else:
    run(f"rm -rf dist-newstyle/", shell=True)

if not ok:
    sys.exit(1)