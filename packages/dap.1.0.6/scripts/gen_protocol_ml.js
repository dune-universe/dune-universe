const Fs = require('fs');
const yargs = require('yargs/yargs')
const { hideBin } = require('yargs/helpers')
const argv = yargs(hideBin(process.argv)).argv
const _ = require('lodash');
const hackSchema = require('./hack_schema');

const schema = hackSchema(JSON.parse(Fs.readFileSync(argv._[0])));

function resolveDef(def) {
  if (def != null && def.$ref) {
    return schema.definitions[def.$ref.replace(/^#\/definitions\//, '')];
  }
  return def;
}

const types = {};
const events = {};
const requests = {};

for (let key of Object.keys(schema.definitions)) {
  let def = schema.definitions[key];
  if (key.endsWith('Event') && key !== 'Event') {
    const event = key.slice(0, -'Event'.length).replace(/^[A-Z]/, c => c.toLowerCase());
    events[event] = {body: resolveDef((def.allOf[1].properties || {}).body)};
  } else if (key.endsWith('Request') && key !== 'Request') {
    const cmd = key.slice(0, -'Request'.length).replace(/^[A-Z]/, c => c.toLowerCase());
    requests[cmd] = requests[cmd] || {};
    requests[cmd].doc = def.allOf[1].description;
    requests[cmd].arguments = resolveDef((def.allOf[1].properties || {}).arguments);
  } else if (key.endsWith('Response') && key !== 'Response' && key !== 'ErrorResponse') {
    const cmd = key.slice(0, -'Response'.length).replace(/^[A-Z]/, c => c.toLowerCase());
    requests[cmd] = requests[cmd] || {};
    requests[cmd].responseBody = resolveDef((def.allOf[1].properties || {}).body);
  } else if (key.endsWith('Arguments')) {
    // Skip
  } else {
    types[key] = def;
  }
}

let out = ``;
function emit(str) {
  out += str;
}

emit(`(** ${schema.description} *)\n`);
emit(`(* Auto-generated from json schema. Do not edit manually. *)\n\n`);
emit(`open Util\n\n`);
emit(`include Debug_protocol_types\n\n`);

const toOcamlName = (() => {
  function toSnakeCase(key) {
    return key.replace(/([a-z])([A-Z]+)/g, (m, g1, g2) => `${g1}_${g2.toLowerCase()}`);
  }

  function capitalize(key) {
      return key.replace(/^[a-z]/, t => t.toUpperCase());
  }

  let KEYWORDS = ["and", "let", "type", "class", "as", "assert", "begin", "class", "constraint", "do", "while", "for", "done", "while", "for", "downto", "for", "else", "if", "end", "exception", "external", "false", "for", "fun", "function", "functor", "if", "in", "let", "include", "inherit", "initializer", "lazy", "let", "match", "method", "module", "mutable", "new", "object", "of", "type", "exception", "open", "or", "private", "rec", "let", "sig", "struct", "then", "if", "to", "for", "true", "try", "type", "val", "virtual", "when", "while", "with", "match", "try"];

  return (key, makeCap) => {
    key = toSnakeCase(key);
    if (makeCap) {
      key = capitalize(key);
    }
    if (KEYWORDS.includes(key)) {
        key += '_';
    }
    return key;
  };
})();

function withBuffer(emit, f) {
  let buf = '';
  f((str) => {
    buf += str;
  });
  emit(buf);
}

function withIndent(emit, f) {
  withBuffer((buf) => {
    emit(buf.split(/\n/g).map(it => it && '  ' + it).join('\n'));
  }, f);
}

let emitLocalModule = () => {
  throw new Error();
};

function emitModule(emit, modId, f) {
  emit(`module ${modId} = struct\n`);
  const prevEmitLocalModule = emitLocalModule;
  emitLocalModule = (modId, f) => {
    withIndent(emit, (emit) => {
      emitModule(emit, modId, f);
      emit('\n');
    });
  };
  withIndent(emit, f);
  emitLocalModule = prevEmitLocalModule;
  emit(`end\n`);
}

function genDoc(str) {
  return `(** ${str.replace(/([{}\[\]@])/g, '\\$1')} *)`;
}

let currentTypeModuleName;

function genType(def, prop, parentDef) {
  if (def.allOf != null || def.type === 'object' || !_.isEmpty(def.enum) || !_.isEmpty(def._enum)) {
    const entry = Object.entries(types).find(it => it[1] === def);
    if (entry != null) {
      const modId =  toOcamlName(entry[0], true);
      emitTypeModule(modId, def);
      if (modId === currentTypeModuleName) {
        return `t`;
      }
      return `${modId}.t`;
    } else {
      const modId = toOcamlName(prop, true);
      emitLocalModule(modId, (emit) => {
        emitTypeDecl(emit, def);
      });
      if (modId === currentTypeModuleName) {
        return `t`;
      }
      return `${modId}.t`;
    }
  }

  if (_.isEqual(_.sortBy(def.type), ['integer', 'string'])) {
    return 'Int_or_string.t';
  }

  if (_.isEqual(_.sortBy(def.type), ['null', 'string'])) {
    return 'string option';
  }

  if (_.isEqual(_.sortBy(def.type), ['array', 'boolean', 'integer', 'null', 'number', 'object', 'string'])) {
    if (parentDef.allOf != null && resolveDef(parentDef.allOf[0]) === types.ProtocolMessage) {
      return `Yojson.Safe.t`;
    }
    return 'Any.t';
  }

  switch (def.type) {
    case 'string': return 'string';
    case 'boolean': return 'bool';
    case 'integer': return 'int';
    case 'number': return 'float';
    case 'array': {
      const itemType = resolveDef(def.items);
      return `${genType(itemType, prop)} list`;
    }
    default:
      throw new Error('Assertion failed');
  }
}

const emittedTypes = new Set();

function emitTypeDecl(emit, def, {generic, isEmitTypeModule} = {}) {
  if (def != null && def.description) {
    emit(genDoc(def.description));
    emit('\n');
  }
  emit(`type t =`);
  if (!isEmitTypeModule && Object.entries(types).some(it => it[1] === def)) {
    const typeName = Object.entries(types).find(it => it[1] === def)[0];
    emit(` ${toOcamlName(typeName)}.t\n`);
    emit(`[@@deriving yojson]`);
  } else if (def != null && def.type === 'object' && def.additionalProperties != null) {
    if (_.isEqual(_.sortBy(def.additionalProperties.type), ['null', 'string'])) {
      emit(` String_opt_dict.t\n`);
    } else if (def.additionalProperties.type === 'string') {
      emit(` String_dict.t\n`);
    } else {
      throw new Error('Assertion failed');
    }
    emit(`[@@deriving yojson]`);
  } else if (def == null || (def.type === 'object' && _.isEmpty(def.properties))) {
    emit(` Empty_dict.t\n`);
    emit(`[@@deriving yojson]`);
  } else if (def.type === 'object' || def.allOf != null) {
    let objDef = def;
    if (def.allOf != null) {
      const def1 = resolveDef(def.allOf[0]);
      const def2 = resolveDef(def.allOf[1]);
      objDef = {
        type: 'object',
        properties: {
          ...def1.properties,
          ...def2.properties,
        },
        required: _.union(def1.required, def2.required),
      };
    }
    emit(` {\n`);
    withIndent(emit, (emit) => {
      for (const [prop, propDef0] of Object.entries(objDef.properties || {})) {
        const propDef = resolveDef(propDef0);
        const mlprop = toOcamlName(prop);
        emit(mlprop);
        emit(` : `);
        const typExp = genType(propDef, prop, def);
        emit(typExp);
        const isOptional = !(objDef.required || []).includes(prop);
        if (isOptional && !typExp.endsWith(' option') && !(generic && ['arguments', 'body'].includes(prop))) {
          emit(` option`);
        }
        if (mlprop !== prop) {
          emit(` [@key "${prop}"]`);
        }
        if (generic && ['arguments', 'body'].includes(prop)) {
          emit(` [@default \`Assoc []]`);
        } else if (generic && prop === 'type') {
          // if (def.allOf[1].properties.event) {
          //   emit(` [@default Type.Event]`);
          // } else if (def.allOf[1].properties.body) {
          //   emit(` [@default Type.Response]`);
          // } else {
          //   emit(` [@default Type.Request]`);
          // }
        } else if (isOptional) {
          emit(` [@default None]`);
        }
        emit(`;`);
        if (propDef0.description) {
          emit(' ');
          emit(genDoc(propDef0.description));
        }
        emit(`\n`);
      }
    });
    emit(`}\n`);
    emit(`[@@deriving make, yojson {strict = false}]`);
  } else if (def.type === 'string' && Array.isArray(def.enum || def._enum)) {
    const isOpen = def._enum != null && def.enum == null;
    const strs = def.enum || def._enum;
    emit(`\n`);
    for (const str of strs) {
      emit(`  | `);
      const mlname = toOcamlName(str.replace(/ /g, '_'), true);
      emit(mlname);
      if (mlname != str) {
        emit(` [@name "${str}"]`);
      }
      emit(`\n`);
    }
    if (isOpen) {
      emit(`  | Custom of string\n`);
    }
    emit(`\n`);
    emit(`let of_yojson = function\n`);
    for (const str of strs) {
      emit(`  | \`String "${str}" -> Ok ${toOcamlName(str.replace(/ /g, '_'), true)}\n`);
    }
    if (isOpen) {
      emit(`  | \`String str -> Ok (Custom str)`);
    }
    emit(`  | _ -> Error (print_exn_at_loc [%here])\n\n`);
    emit(`let to_yojson = function\n`);
    for (const str of strs) {
      emit(`  | ${toOcamlName(str.replace(/ /g, '_'), true)} -> \`String "${str}"\n`);
    }
    if (isOpen) {
      emit(`  | Custom str -> \`String str`);
    }
  }
  emit(`\n`);
}

function emitTypeModule(typeName, def) {
  if (emittedTypes.has(def)) {
    return;
  }
  emittedTypes.add(def);
  const prevTypeModuleName = currentTypeModuleName;
  currentTypeModuleName = toOcamlName(typeName);
  withBuffer(emit, (emit) => {
    const modName = toOcamlName(typeName, true);
    emitModule(emit, modName, (emit) => {
      emitTypeDecl(emit, def, {generic: ['Event', 'Request', 'Response'].includes(modName), isEmitTypeModule: true});
    });
    emit('\n');
  });
  currentTypeModuleName = prevTypeModuleName;
}

function emitEventModule(event, {doc, body}) {
  withBuffer(emit, (emit) => {
    if (doc) {
      emit(genDoc(doc));
      emit('\n');
    }
    emitModule(emit, toOcamlName(`${event}_event`, true), (emit) => {
      emit(`let type_ = "${event}"\n`);
      emit('\n');
      emitModule(emit, toOcamlName('Payload', true), (emit) => {
        emitTypeDecl(emit, body);
      });
    });
    emit('\n');
  });
}

function emitRequestModule(command, {doc, arguments, responseBody}) {
  withBuffer(emit, (emit) => {
    if (doc) {
      emit(genDoc(doc));
      emit('\n');
    }
    emitModule(emit, toOcamlName(`${command}_command`, true), (emit) => {
      emit(`let type_ = "${command}"\n`);
      emit('\n');
      emitModule(emit, toOcamlName('Arguments', true), (emit) => {
        emitTypeDecl(emit, arguments);
      });
      emit('\n');
      emitModule(emit, toOcamlName('Result', true), (emit) => {
        emitTypeDecl(emit, responseBody);
      });
    });
    emit('\n');
  });
}

for (const [typeName, def] of Object.entries(types)) {
  emitTypeModule(typeName, def);
}
for (const [eventType, event] of Object.entries(events)) {
  emitEventModule(eventType, event);
}
for (const [command, request] of Object.entries(requests)) {
  emitRequestModule(command, request);
}

process.stdout.write(out);
