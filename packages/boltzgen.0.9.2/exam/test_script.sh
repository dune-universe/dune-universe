#!/bin/bash

#Script de test pour un fichier .c avec un seul exercice, contre un fichier de référence
#

#Tous les repertoires doivent etre dans le meme repertoire parent

#Variables d'environement
#LOGIN - login
#PROJECT_NAME - project_name
#TEST_DIR - the tests dir
#WORK_DIR - the student file is in this dir. Any file should be written in this dir
#REF_DIR - the reference directory
#SCRIPT_DIR - the script directory
#STUDENT_DIR - the student directory
#TESTS_GROUPS - space separated tests groups
#AUTO_RUN - set if project has been launched from a browser
#USE_REF - set if a reference should be used
#RESULT_FILE - result file
#ERROR_FILE - error file


#TIMEOUT - Seconde maximum d'execution
#TIMEOUT=3
#TEST_ARGS - 1 si le fichier de test est un argument à donner au programme (separe par des retours à la ligne)
#          - 0 si le fichier de test est donne en entree standard
#TEST_ARGS=0
#COMP_OPT - compilation options
#LINK_OPT - link options


#INPUT - Input tests
INPUT=Input
#OUTPUT - Output tests
OUTPUT=Output
#STUDENT_FILE - Nom du fichier de l'eleve et du fichier de reference
STUDENT_FILE=rendu.ml
#CORRECTION_FILE - fichier de correction
CORRECTION_FILE="$RESULT_FILE"
#EXEC_FILE - Nom de l'executable
EXEC_FILE="prog.exe"
#TIME_OUT_EXE - Programme de tiemout
TIME_OUT_EXE=/usr/bin/timeout
TIMEOUT=2

RED='\033[0;31m'
GREEN='\033[1;32m'
NC='\033[0m' # No Color

#cp -r /proj_dir ~/proj_dir
#cd ~/proj_dir

error=""

echo "" > "$WORK_DIR"/"$CORRECTION_FILE"

function replaceSpeChar {
    local cool="$1"
        local without="${cool//$'\r\n'/\\n}"
    without="${without//$'\n'/\\n}"
    echo "$without"
}

function standardTest {
    #$1 - Exercice name
    #$2 - result file
    #$3 - exe name

    error1=""
    echo "$1" >> "$WORK_DIR"/"$2"

    comp_ok=1

    for test in "$TEST_DIR"/"$1"/"$INPUT"/*
    do
                testname=$(basename $test)

                if [ "$comp_ok" -ne 0 ]
                then
                        error1=""
                        res=1
                        rm -rf "$WORK_DIR"/error_file "$WORK_DIR"/filestudent
			cp "$WORK_DIR"/"$STUDENT_DIR"/rendu.ml rendu.ml
			cp "$WORK_DIR"/"$REF_DIR"/rendu.ml reference.ml
                                ("$TIME_OUT_EXE" "$TIMEOUT" ocaml -I "$WORK_DIR"/Scripts unix.cma gen_test_lib.cma unix.cma "$test" rendu.ml reference.ml) > "$WORK_DIR"/error_file 2> "$WORK_DIR"/filestudent
                        res_code=$?

                        if [ $res_code -ne 0 ] || [ -s "$WORK_DIR"/error_file ]
                        then

                            er=$(replaceSpeChar "$(cat "$WORK_DIR"/error_file)")
                            ou=$(replaceSpeChar "\n$(cat "$WORK_DIR"/filestudent)\n")
                                if [ $res_code = 124 ]
                                then
                                        error1="Timeout sur "$testname" - Erreur : (Trop d'appel récursif ?) - Test "
                                else
                                        error1="Problème d'execution sur "$testname" - Erreur : ""$er"" ""$ou"" - Test "
                                fi
                                res=0
                        else
                                #ocaml str.cma -I "$WORK_DIR"/Scripts gen_test_lib.cma "$test" "$WORK_DIR"/"$REF_DIR"/rendu.ml "$WORK_DIR"/"$REF_DIR"/reference.ml > "$WORK_DIR"/fileref
                                echo "" > "$WORK_DIR"/fileref

                                #diff "$WORK_DIR"/filestudent "$WORK_DIR"/fileref > /dev/null 2> /dev/null
                                if [ -s "$WORK_DIR"/filestudent ]
                                then
                                    er=$(replaceSpeChar "\n$(cat $WORK_DIR/filestudent)\n")
                                    error1="Mauvais résultat sur "$testname" - $er"
                                    res=0
                                fi
                                rm "$WORK_DIR"/fileref 2> /dev/null
                        fi
                        rm "$WORK_DIR"/filestudent 2> /dev/null
                else
                        res=0
                fi

                if [ -z "$AUTO_RUN" ]
                then
                        if [ "$res" -eq "0" ]
                        then
                           echo -e "${RED}$testname KO${NC}"
                        else
                           echo -e "${GREEN}$testname OK${NC}"
                        fi
                fi

                echo "$testname" "$res" "$error1" >> "$WORK_DIR"/"$2"
    done

    echo "" >> "$WORK_DIR"/"$2"

    rm "$WORK_DIR"/error_file 2> /dev/null
}

# cd Reference
# tar -zxvf "$2" > /dev/null
# cd ..


# decomp_ok=1
# error=""
# cd "$1"
# tar -zxvf "$2" 2> error_file 1> /dev/null
# if [ $? -ne 0 ]
# then
    # er=$(replaceSpeChar "$(cat error_file)")
    # error="Problème de décompression du fichier de rendu\n""$er""\n\n"
    # decomp_ok=0
# fi

# cd ..

echo "$PROJECT_NAME" > "$WORK_DIR"/"$CORRECTION_FILE"
echo "$LOGIN" >> "$WORK_DIR"/"$CORRECTION_FILE"
echo "" >> "$WORK_DIR"/"$CORRECTION_FILE"

comp_ok=1
#ocaml $COMP_OPT "$WORK_DIR"/"$REF_DIR"/*.ml $LINK_OPT -o "$WORK_DIR"/"$REF_DIR"/"$EXEC_FILE" 2> "$WORK_DIR"/error_file
if [ $? -ne 0 ]
then
        er=$(replaceSpeChar "$(cat $WORK_DIR/error_file)")
        error1="Problème de compilation sur le fichier de référence: ""$er"
        echo "$error1" > "$WORK_DIR"/"$ERROR_FILE"
        comp_ok=0
fi

if [ "$comp_ok" -ne 0 ]
then
        ARRAY=($TESTS_GROUPS)
        for i in "${ARRAY[@]}"
        do
                standardTest "$i" "$CORRECTION_FILE" "$EXEC_FILE"
        done
fi
