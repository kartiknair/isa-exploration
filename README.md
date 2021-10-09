# ISA Exploration

This repository contains the source code for two tiny instruction sets (one RISC, one CISC), virtual machines for both, a compiler for a C-level language to both those instruction sets, and an online playground to show the differences between the two instruction sets.

## Instructions to build playground locally

Building the playground requires [`wasm-pack`](https://rustwasm.github.io/wasm-pack/installer/) and Node.js+NPM to be installed locally on your system.

```
wasm-pack build
cd www
npm install # only required the first time
npm start
```
