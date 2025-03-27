# Restrictor Case Study Generator

## Setup

TODO

I suggest using GHCUp to install the Haskell toolchain. Then, run `stack update`
(anywhere) to update Stack's repository cache, then (in project root directory)
`stack build`.

## Usage

Currently only executable through stack (will be improved soon).

Run

    stack exec cav2025-cedar-restrict-gclassroom-exe -- --help

for usage instructions. For example, for the default GClassroom case study,

    stack exec cav2025-cedar-restrict-gclassroom-exe -- gc

## Directory

- Entity store generation is performed in `app/${CASE_STUDY}/GenEntities.hs`
- Log generation is performed in `app/${CASE_STUDY}/GenLogs.hs`
- The Cedar schema file is `assets/${CASE_STUDY}/schema.cedarschema`
- The Cedar policy file is `assets/${CASE_STUDY}/policies.cedar`

## Conventions

- In the `GenLogs.hs` files: the "p" prefix indicates a function that produces a
  request that should be allowed ("p"ermited) in both the original and tightened
  policy; the "eop" prefix indicates a function producing a request that should
  be allowed by the original policy, but not in the tightened policy
  ("e"xercised "o"ver"p"rivilege)
