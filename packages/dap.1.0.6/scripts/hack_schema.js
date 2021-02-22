module.exports = hackSchema;

function hackSchema(schema) {
  schema.definitions.Variable.properties.__vscodeVariableMenuContext = { type: 'string' };
  return schema;
}
