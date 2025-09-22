import { $apply1, dsl_main } from "./generated_stage2.js";

Error.stackTraceLimit = Infinity;

function main() {
  const start = Date.now();
  $apply1(dsl_main, []);
  const end = Date.now();
  console.log((end - start) / 1000);
}

main();
