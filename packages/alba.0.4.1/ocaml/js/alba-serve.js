#!/usr/bin/env node

var http = require('http');
var fs   = require('fs');
var path = require('path');


function file_path (arr) {
    var name = process.cwd()
    for (var i = 0; i < arr.length; i++){
        name = path.join(name,arr[i])
    }
    return name
}


function respond_404 (res, message) {
    res.writeHead(404, { "Content-Type" : "text/plain" });
    res.end(message);
}



function get_file (file_name, content_type, res) {
    var rs = fs.createReadStream(file_name);
    var first = true;
    rs.on('error', e => {
        console.log(e);
        res.writeHead(404, { "Content-Type" : content_type });
        res.end("not found");
    });
    rs.on('readable', function () {
        var d = rs.read();
        if (d) {
            if (first){
                res.writeHead(200, { "Content-Type" : content_type });
                first = false;
            }
            res.write(d.toString('utf8'));
        }
    });
    rs.on('end', function () {
        res.end();
    });
}

function write_file (file_name,request,response) {
    var ws = fs.createWriteStream(file_name)
    ws.on('error', e => {
        console.log(e);
        respond_404(res, 'cannot open ' + file_name)
    })
    request.on('data', data => {ws.write(data)})
    request.on('end', function() {ws.end(); response.end()})
}

function remove_file (file_name, res) {
    fs.unlink (file_name, err => {
        if (err)
            respond_404(res,err.message)
        else
            res.end()
    })
}

function cm_lib_path (name) {
    return path.join(__dirname, 'node_modules', 'codemirror', 'lib', name)
}
function cm_lint_path (name) {
    return path.join(__dirname, 'node_modules', 'codemirror', 'addon', 'lint', name)
}
function cm_edit_path (name) {
    return path.join(__dirname, 'node_modules', 'codemirror', 'addon', 'edit', name)
}


function handle_post (req,res) {
    var arr = req.url.split("/")
    console.log(arr)
    arr.shift() // must start with '/'
    console.log(arr)
    if (arr.length < 2) {
        console.log("post request without arguments")
        res.end()
    } else {
        var command = arr.shift()
        if (command == 'write_file')
            write_file (file_path(arr), req, res)
        else if (command == 'remove_file')
            remove_file (file_path(arr), res)
        else {
            console.log ('unknown post comand ' + command)
        }
    }
}

function read_directory (args, res) {
    fs.readdir(file_path(args), (err,files) => {
        if (err)
            respond_404 (res, err.message)
        res.end(files.join(','))
    })
}

function file_stat (args, res) {
    fs.stat (file_path(args), (err,stat) => {
        if (err)
            respond_404 (res, err.message)
        res.end(JSON.stringify(stat))
    })
}

function get_program_file (name, res) {
    if (name == "") {
        get_file (path.join(__dirname, 'web_gui.html'), "text/html", res);
    } else if (name == "web_gui.css") {
        get_file(path.join(__dirname, name),
                 'text/css',
                 res);
    } else if (name == "codemirror.js") {
        get_file(cm_lib_path(name),
                 'text/javascript',
                 res);
    } else if (name == "codemirror.css") {
        get_file(cm_lib_path(name),
                 'text/css',
                 res);
    } else if (name == "lint.css") {
        get_file(cm_lint_path(name),
                 'text/css',
                 res);
    } else if (name == "lint.js") {
        get_file(cm_lint_path(name),
                 'text/js',
                 res);
    } else if (name == "matchbrackets.js") {
        get_file(cm_edit_path(name),
                 'text/js',
                 res);
    } else if (name == "closebrackets.js") {
        get_file(cm_edit_path(name),
                 'text/js',
                 res);
    } else {
        get_file (path.join(__dirname,name), "text/plain", res);
    }
}

function handle_get (req, res) {
    var args = req.url.split("/")
    args.shift();
    var name = args.shift();
    if (args.length == 0)
        get_program_file (name, res)
    else if (name == 'read_file')
        get_file (file_path(args), "text/plain", res)
    else if (name == 'read_directory')
        read_directory (args, res)
    else if (name == 'file_stat')
        file_stat (args, res)
    else {
        console.log("unknown get request " + name)
        console.log("args: " + args)
        res.end()
    }

}


function handle_request (req, res) {
    console.log(req.method + "   " + req.url);
    if (req.method == "GET")
        handle_get(req,res)
    else if (req.method == 'POST') {
        handle_post (req,res)
    } else {
        respond_404(res, 'unknown request method ' + req.method)
    }
}

var server = http.createServer(handle_request);

server.on ('error', function(e) {
    console.log("Cannot listen on port 8080");
});

function alba_directory () {
    return path.join(process.cwd(), ".alba");
}

fs.stat(alba_directory(),
        (err,stats) => {
            if (err) {
                console.log("The directory \"" + process.cwd() +
                            "\" is not a valid Albatross directory");
            } else {
                server.listen(8080);
            }
        });


console.log("listening on localhost:8080 ...");
