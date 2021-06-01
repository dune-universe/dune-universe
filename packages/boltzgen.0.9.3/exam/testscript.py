import sys, json, os, math, subprocess, re
from shutil import copyfile

data = json.load(sys.stdin)
params = data["config"]["params"]
params_opt = data["config"]["params_opt"]
os.environ["LOGIN"]= data["work"][0]["username"]

#name_ref = "refdir/{0}".format(params['exec']['value'])
#name_work = "workdir/{0}".format(params['exec']['value'])

def escapeShell(str):
    return str.replace('\\', '\\\\').replace('"', '\\"')

def escapeShellSingle(str):
    return str.replace("'", "'\\''")

def read_file(path, default, include):
    msg = default
    if os.path.exists(path):
        fp = open(path, 'r', errors='replace')
        if not include:
            msg = fp.read()
        else:
            msg = "{0} {1}".format(msg, fp.read())
        fp.close()
        os.remove(path)
    return msg

cwd = os.getcwd()
os.chdir(os.environ['WORK_DIR'])
os.mkdir('{0}/refdir'.format(os.environ['WORK_DIR']))
all_ref_files = ""

os.mkdir('{0}/workdir'.format(os.environ['WORK_DIR']))
all_work_files = ""
for elt in data["work"]:
    all_work_files = "{0} workdir/{1}".format(all_work_files, elt["filename"])
    fp = open("workdir/rendu.ml", 'w')
    fp.write("open Question_type\n")
    fp.write(elt["code"])
    fp.close()

os.chdir("{0}/workdir".format(os.environ['WORK_DIR']))
res = os.system("ocamlc -c -I ../refdir -I /opt/ocamlruntime rendu.ml 2> error.txt 1>&2")
if res != 0:
    msg = read_file('error.txt', 'unable to retrieve the error', False)
    output = {"global":{"total":"0%", "pass" : "0", "error" : {"type" : "comp", "msg" : "{0}".format(msg)}}, "details":[]}
    json.dump(output, sys.stdout)
    exit()
os.chdir(os.environ['WORK_DIR'])

regexp_type=re.compile('sig(.*)end.*match:(.*)is not included in(.*)File.*File.*',re.S)
regexp_missing=re.compile('The value (.*) is required',re.S)
regexp_output=re.compile('(.*) = (.*) à la place de (.*)\n',re.S)
#match:(.*)is not included in(.*)File.*
details = []
nb_correct = 0
for testi in range(1,1+int(params["nbtest"]["value"])):

    #sys.stderr.write("bash -c 'echo '\\''{0}'\\'' | ./{1}; true' 2> error.txt | head -c 100000 > output_ref.txt".format(escapeShellSingle(escapeShellSingle(elt["input"])), name_ref))
    #exit()

    os.chdir("{0}".format(os.environ['WORK_DIR']))
    
    testEffort = "20"
    if 'testEffort' in params_opt :
        testEffort = params_opt["testEffort"]["value"]
    testMaxValue = "40"
    if 'maxValue' in params_opt :
        testMaxValue = params_opt["maxValue"]["value"]
    fp = open("test.ml", 'w')
    #fp.write('Gen_test_lib.gen_test_d 50 50 "')
    testelt="Gen_test_lib.gen_qbank ~really_test:false \"{0}\" {1};;#use \"t.ml\"".format(params["qbank"]["value"],testi)
    fp.write(testelt)
    #fp.write('";; #use "t.ml";;');
    fp.close()
        
    #p = subprocess.Popen("ls /opt/ocamlruntime/ 2> error.txt | head -c 100000 > output_ref.txt", shell=True)
    copyfile("workdir/rendu.ml", "rendu.ml")
   
    p = subprocess.Popen("ocaml unix.cma -I ../refdir -I /opt/ocamlruntime gen_test_lib.cma test.ml rendu.ml reference.ml 2> error.txt > output_ref.txt".format(cwd), shell=True)
    #p = subprocess.Popen("cat test.ml > error.txt", shell=True)
    killed=False
    
    #p = subprocess.Popen("ocaml -I {0} unix.cma gen_test_lib.cma test.ml 2> error.txt | head -c 100000 > output_ref.txt".format(cwd), shell=True)
    try:
        res = p.wait(4)
    except subprocess.TimeoutExpired:
        p.kill()
        killed=True
        details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "Test", "error" : {"type" : "exec", "msg" : "Timeout! Le test est trop long à s'executer."}})
        continue
        

    msg = read_file('error.txt', '', True)
    msg2 = read_file('output_ref.txt', '', True)
    
    if msg2.strip() !='' :
        m = re.search(regexp_type,msg2)
        if m :
            details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "Test", "error" : {"type" : "misc", "msg" : "Erreur de type: '{0}' à la place de '{1}'".format(m.group(2),m.group(3))}})
        else :
            m2 = re.search(regexp_missing,msg2)
            if m2 :
                details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "Test", "error" : {"type" : "misc", "msg" : "Fonction manquante: {0}".format(m2.group(1))}})
            else :
                details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "Test", "error" : {"type" : "misc", "msg" : "Erreur de type: {0}".format(msg2)}})
        continue
    
    if msg.strip() != '' :
        m2 = re.search(regexp_output,msg)
        if m2 :
            details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "{0}".format(m2.group(1)), "error" : {"type" : "output", "given" : "{0}".format(m2.group(2)), "expected": "{0}".format(m2.group(3))}})
            continue
        else :
            details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "Test", "error" : {"type" : "misc", "msg" : "Mauvais résultat: {0}".format(msg)}})
        continue
    
    
    if res != 0 :
        details.append({"name" : "Test {0}".format(testi), "pass" : "0", "input" : "Test", "error" : {"type" : "misc", "msg" : "Unknown Error"}})
        continue
    
    
    os.chdir(os.environ['WORK_DIR'])

    details.append({"name" : "Test {0}".format(testi), "pass" : "1", "input" : "Test"})
    nb_correct = nb_correct + 1

ratio = 1
if len(details)>0 :
    ratio = int(math.floor(nb_correct/len(details)*100))
if nb_correct == len(details):
    passs = "1"
else:
    passs = "0"

res = {"examiner":os.environ['NAME'],
       "global":{"total":"{0}%".format(ratio), "pass" : "{0}".format(passs), "error" : {"type" : "misc", "msg" : "there were some errors in the tests"}, "success" : {"type" : "misc", "msg" : "You pass all the tests!"}},
      "details": details
      }

json.dump(res, sys.stdout)
