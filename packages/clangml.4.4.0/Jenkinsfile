def opam_installations(ocamlversions, url) {
    def pwd = sh (
        script: 'echo $PWD',
        returnStdout: true
    ).trim()
    def branches = [:]
    for (i in ocamlversions) {
        def ocamlversion = i
        branches[ocamlversion] = {
            node {
                sh """
                    docker run --rm -v $pwd/src:/clangml \
                        ocaml/opam2:$ocamlversion \
                        /clangml/ci-scripts/opam-pin_and_install.sh $url
                    """
            }
        }
    }
    parallel branches
}

pipeline {
    agent {
        label 'slave'
    }
    stages {
        stage('Prepare') {
            steps {
                sh 'mkdir src'
                sh 'mv * src/ || true'
                sh '''
                    eval $(opam env) && \
                    cd src && \
                    rm -rf bootstrap/ && \
                    tar -xf ~/bootstrap.tar.xz && \
                    m4/download.sh && \
                    ./autogen.sh
                   '''
            }
        }
        stage('Configure') {
            steps {
                timeout(time: 2, unit: 'MINUTES') {
                    sh '''
                        eval $(opam env) && \
                        mkdir build && cd build && \
                        ../src/configure \
                            --with-llvm-config=/media/llvms/12.0.0/bin/llvm-config
                       '''
                }
            }
        }
        stage('Build') {
            steps {
                timeout(time: 3, unit: 'MINUTES') {
                    sh 'make -C build clangml'
                    sh 'make -C build clangml.opam && cp build/clangml.opam src/'
                    sh 'make -C build tools/stubgen'
                    sh 'make -C build tools/norm_extractor'
                    sh 'make -C build tools/generate_attrs'
                }
            }
        }
        stage('Generate attributes') {
            steps {
                timeout(time: 3, unit: 'MINUTES') {
                    sh '''
                       eval $(opam env) && \
                       cd build && \
                       dune exec -- tools/generate_attrs/generate_attrs.exe \
                         --llvm-config /media/llvms/12.0.0/bin/llvm-config \
                         "../src/bootstrap/" && \
                       cp ../src/bootstrap/attributes.ml clangml && \
                       cp ../src/bootstrap/libclang_extensions_attrs.inc \
                         clangml && \
                       cp ../src/bootstrap/libclang_extensions_attrs_headers.inc \
                         clangml
                       '''
                }
            }
        }
        stage('Generate stubs') {
            steps {
                script {
                    def pwd = sh (
                        script: 'echo $PWD',
                        returnStdout: true
                    ).trim()
                    def llvm_versions = sh (
                        script: 'ls -1 /media/llvms',
                        returnStdout: true
                    ).split('\n').toList()
                    llvm_versions.retainAll { it =~ /[0-9][.][0-9][.][0-9]/ }
                    def branches = [:]
                    for (i in llvm_versions) {
                        def llvm_version = i
                        def llvm_dir = "/media/llvms/$llvm_version"
                        def llvm_config = "$llvm_dir/bin/llvm-config"
                        def bootstrap_dir = "src/bootstrap/$llvm_version"
                        def include_dir =
                            "$llvm_dir/lib/clang/$llvm_version/include"
                        def cc
                        def cxx
                        branches[llvm_version] = {
                            node {
                                sh """
                                    eval \$(opam env) && \
                                    cd $pwd && \
                                    mkdir -p $bootstrap_dir && \
                                build/_build/default/tools/stubgen/stubgen.exe \
                                        --cc=-I,build,-I,$include_dir \
                                        --llvm-config=$llvm_config \
                                        $bootstrap_dir/
                                   """
                                sh """
                                    eval \$(opam env) && \
                                    cd $pwd && \
                                    mkdir $llvm_version/ && \
                                    cd $llvm_version/ && \
                                    ../src/configure \
                                        --with-llvm-config=$llvm_config && \
                                    make clangml
                                   """
                                sh """
                                    eval \$(opam env) && \
                                    cd $pwd/$llvm_version/ && make test
                                   """
                                sh """
                                    eval \$(opam env) && \
                                    cd $pwd/$llvm_version/ && \
                                    make tools/stubgen && \
                                    mkdir current && \
                                    _build/default/tools/stubgen/stubgen.exe \
                                        --cc=-I,build,-I,$include_dir \
                                        --llvm-config=$llvm_config \
                                        current/ && \
                                    diff clangml/clang__bindings.ml \
                                      current/clang__bindings.ml && \
                                    diff clangml/clang__bindings.mli \
                                      current/clang__bindings.mli && \
                                    diff clangml/clang_stubs.c \
                                      current/clang_stubs.c
                                   """
                            }
                        }
                    }
                    parallel branches
                }
                sh 'cd src && tar -cf bootstrap.tar.xz bootstrap/'
                archiveArtifacts artifacts: 'src/bootstrap.tar.xz',
                    fingerprint: true
            }
        }
        stage('Commit to bootstrap branch') {
            when { branch 'master' }
            steps {
                script {
                    def commit = sh (
                        script: 'git rev-parse HEAD',
                        returnStdout: true
                    ).trim()
                    sh 'git checkout origin/bootstrap'
                    sh 'tar -xf src/bootstrap.tar.xz'
                    sh 'git add bootstrap'
                    sh """
                        git commit -m 'generated files for commit $commit' || \
                        true
                       """
                    sh 'git push origin HEAD:bootstrap'
                }
            }
        }

        stage('opam installation') {
            when { branch 'master' }
            steps {
                script {
                    opam_installations(
                        ["4.08", "4.09", "4.10", "4.11"],
                        "file:///clangml/")
                }
            }
        }

        stage('Commit to snapshot branch') {
            when { branch 'master' }
            steps {
                sh 'src/ci-scripts/commit-snapshot-branch.sh'
            }
        }

        stage('opam installation from snapshot') {
            steps {
                script {
                    def repo = env.JOB_NAME == "perso/master" ? "tmartine" : "memcad"
                    opam_installations(
                        ["4.11"],
"https://gitlab.inria.fr/$repo/clangml/-/archive/snapshot/clangml-snapshot.tar.gz")
                }
            }
        }

        stage('Extract norms') {
            parallel {
                stage('c++14') {
                    steps {
                        sh '''
                            path=$PWD &&
                            cd $HOME/cplusplus/c++14 &&
                            build_dir=$path/build target_dir=$path std=c++14 \
                                $path/src/ci-scripts/extract_norm.sh
                        '''
                    }
                }
                stage('c++17') {
                    steps {
                        sh '''
                            path=$PWD &&
                            cd $HOME/cplusplus/c++17 &&
                            build_dir=$path/build target_dir=$path std=c++17 \
                                $path/src/ci-scripts/extract_norm.sh
                        '''
                    }
                }
                stage('c++20') {
                    steps {
                        sh '''
                            path=$PWD &&
                            cd $HOME/cplusplus/c++20 &&
                            build_dir=$path/build target_dir=$path std=c++20 \
                                $path/src/ci-scripts/extract_norm.sh
                        '''
                    }
                }
            }
        }

        stage('Commit to norms branch') {
            when { branch 'master' }
            steps {
                sh 'git checkout origin/norms'
                sh 'cp build/norm_c++14.ml norms/'
                sh 'cp build/norm_c++17.ml norms/'
                sh 'cp build/norm_c++20.ml norms/'
                sh 'git add norms/*'
                sh '''
                    git commit -m \
                      "generated files for commit `git rev-parse master`" || \
                    true
                '''
                sh 'git push origin HEAD:norms'
            }
        }
    }
    post {
        failure {
            mail to: 'Thierry.Martinez@inria.fr',
                subject: "ClangML CI failure: ${currentBuild.fullDisplayName}",
                body: "Something is wrong with ${env.BUILD_URL}"
        }
        changed {
            mail to: 'Thierry.Martinez@inria.fr',
                subject:
                  "ClangML CI status changed: ${currentBuild.fullDisplayName}",
                body: "Something changed with ${env.BUILD_URL}"
        }
    }
}
