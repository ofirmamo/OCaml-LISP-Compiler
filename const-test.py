# Imports...
from __future__ import print_function
from subprocess import call
import os
import sys

def clean_workspace(file):
	# Delete files
	files_ext = ["{}.s".format(file), "{}.o".format(file), "{}".format(file), "{}.out".format(file), "{}.chez.out".format(file), "{}.check".format(file), "{}.ans".format(file)]
	for name in files_ext:
		os.remove(name)

# path - path to directory which have .scm files
# path_to_compiler - path to compiler dir.
path = sys.argv[2]
path_to_compiler = sys.argv[1]

# files list of files of the SCMs directory
files = os.listdir(path)

# Collecting valid files, assuming .scm extension..
scm_files = []
for name in files:
	if ".scm" in name:
		scm_files.append(name.replace(".scm", ""))
		
# Debug print..
print("List of files:")
for name in scm_files:
	print("   - {}".format(name))
	
# Change directory to SCMs files..
os.chdir(path)
cwd = os.getcwd()

# Main loop compile scheme -> compile assembly -> link with gcc, currently using -no-pie -> 
# -> execute -> collect answer -> execute via Chez Scheme -> collect answer -> is_equal? -> clean..
for file in scm_files:
	# Compile scheme
	exit_code = os.system("cd {}; ocaml compiler.ml {}/{}.scm > {}/{}.s".format(path_to_compiler,cwd, file, cwd, file))
	if (exit_code >> 8) != 0:
		print("Compiler.ml ERROR :: Exit code is {} for file {}.scm".format((exit_code >> 8), file))
		sys.exit(1)
		
	# Compile Assembly
	exit_code = os.system("cd {}; nasm -f elf64 -o {}/{}.o {}/{}.s".format(path_to_compiler, cwd, file, cwd, file))
	if (exit_code >> 8) != 0:
		print("NASM ERROR :: Exit code is {} for file {}.s".format((exit_code >> 8), file))
		sys.exit(1)
		
	# Link with GCC Using -no-pie for now...
	exit_code = os.system("gcc -m64 -no-pie -g  -o {} {}.o".format(file, file))
	if (exit_code >> 8) != 0:
		print("GCC ERROR :: Exit code is {} for file {}.o".format((exit_code >> 8), file))
		sys.exit(1)
		
	# Execute
	exit_code = os.system("./{} > {}.out".format(file, file))
	
	# Collect answer..
	out = ""
	with open("{}.out".format(file), 'r') as out_file:
		out = out_file.read().replace('\n', ' ')
		
	# Execute Via Chez Scheme, and collect answer..
	exit_code = os.system("(scheme -q < {}.scm) > {}.chez.out".format(file, file))
	chez_out = ""
	with open("{}.chez.out".format(file), 'r') as out_file:
		chez_out = out_file.read().replace('\n', ' ')
		
	# is_equal?
	exec_line = "(equal? '({}) '({}))".format(out.strip(), chez_out.strip())
	os.system("echo \"{}\" > {}.check; (scheme -q < {}.check) > {}.ans".format(exec_line,file, file, file))
	with open("{}.ans".format(file), 'r') as out_file:
			if (out_file.read().strip() == "#t"):
				print("Congratulations, the test for {}.scm passed!".format(file))
				clean_workspace(file)
			else:
				print("Oh no, test for {}.scm failed workscpare will not clean for debug...".format(file))
				sys.exit(1)
		
 