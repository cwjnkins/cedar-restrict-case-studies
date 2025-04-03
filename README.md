# Restrictor Case Study Generator

## Overview and Usage

### Terminology
**Families of datasets**
Case studies are generated as "families", which are sets of related entity
stores and logs, to faciliate the evaluation of how well Restrictor scales. Each
member of a family is associated with a size. At a first approximation, for two
members `m1` and `m2` of the same family such that the size of `m1` < size of
`m2`, every entity appearing in the store for `m1` appears in the store for
`m2`, and every log entry appearing in the log for `m1` appears in the log of
`m2**.

**Representation of exercised privileges in logs**

Each case studies for Restrictor are structured around two policies:
- an implicit "ideal" policy, which we wish Restrictor to discover
- an explicit "actual" policy, deliberately written to permit more requests than
  the ideal policy. 

We maintain a bijection between the rules of the ideal and actual policies, such
that:
- permit (resp. deny) rules of the ideal policy correspond to permit (resp.
  deny) rules of the actual policy;
- for each permit rule of the ideal policy, every request accepted by the rule
  is accepted by its corresponding rule in the actual policy;
- the bijection relating deny rules between the two policies is identity.

As an example, in the Google classroom case study, the ideal policy does not
permit TAs to ever edit assignments, but the actual policy does; the ideal
policy permits teachers to edit assignments, and so does the actual policy.

For each rule of a policy (either ideal or actual), call the set of all requests
(triples `(principal, action, resource)`) for which the rule applies, **and**
for which the policy accepts the request, the *privilege* afforded by the rule.
For each pair of rules `(r1,r2)` where `r1` is a rule from the ideal policy and
`r2` is the corresponding rule from the actual policy:
- the **valid privilege** is the privilege of `r1`;
- the **actual privilege** is the privilege of `r2`;
- the **over privilege** is the set difference of the privilege of `r2` and
  `r1`.

We call such pairs of rules *corresponding rules,* with `r1` the *ideal rule*
and `r2` the *actual rule*. Henceforth, we leave implicit quantification over
corresponding rules implicit when this is unambiguous.

- The **privilege representation** of a rule `r` in a log is the number of
  requests in that log that are members of the privilege of `r`.
- The **valid privilege representation** and **over privilege representation**
  are resp. the privilege representations for the valid privilege of `r1` and
  the over privilege of `r2` in a log.

**Shared command line arguments**

The case-study generator is multi-mode, with each mode corresponding to a case
study (`gc` for Google classroom, `pm` for project management, and `hc` for
HotCRP). Each mode takes case-study specific arguments, and produces a family of
entity stores and logs. Some command line arguments are shared across all modes,
and are important for understanding the process for entity and log family
generation.

- `--size=INT`

  Required to appear at least once, and can appear multiple times. Each
  occurrence specifies the size associated with some family member, with the
  number of family members determined by the number of (distinct) occurrences of
  the size argument.

- `--seed`

  Sets the seed for PRNG for reproducibility (default: `2025`).

-- `--priv-rep-ratio=NUM`

   Upper bound on the ratio of the actual privilege representation to the actual
   privilege for each corresponding pair of rules. It is an upper bound due to
   - rounding down
   - possible duplicates (less likely if this is small --- keep the birthday
     paradox in mind!)

   Note that for some case studies, this general default may be overridden for
   some corresponding pairs.

-- `--over-priv-percent=INT`

   An upper bound on the percentage of the actual privilege representation that
   is over privilege representation. Values range from 0 to 49.


### Entity Generation

Each case study has some invariants imposed on entity generation to make the
synthetic datasets more realistic. For example, for the Google classroom case
study there can never be courses for which there is no teacher (we make the
simplifying assumption that each course has exactly one teacher). The general
organizing principle used across case studies for generating entity stores is to
make a distinction between "primary", "secondary", and (optionally) "tertiary"
entity types.

- A *primary entity type* is one for which the number of entities generated of
  that type is controlled directly by the family size and (optionally) a ratio
  argument.

  For the Google classroom example, these entity types are: students, teachers,
  and TAs (technically, Staff and Role, but we informally treat teachers and TAs
  as separate entity types here).
- A *secondary entity type* is one for which the realism-maintaining invariants
  require that the number of such entities be a (random) function of the number
  of entities of one or more primary or secondary entity types. The command line
  interface allows the the number of such entities to be influenced by an upper
  bound on coefficients

  For the Google classroom example, these entity types are:
  - courses: each teacher teaches a random number of courses from `1` to
    `max-teacher-courseload`
  - assignments: each course has a random number of assignments from `1` to
    `max-assignments-per-course`

- Finally, a *tertiary entity type* is one for which invariants and the
  relations between primary and secondary entity types completely determines the
  number of entities.

  For the Google classroom example, only grades fall into this category (each
  course corresponds to a disjoint set of grades, where each grade corresponds
  to a pair consisting of an assignment for the course and a student enrolled in
  the course. Note that disjointness of course grades is a consequence of the
  imposed invariant that course assignments are disjoint for different courses).

Of course, the entity store also stores the *relations* between entities, not
just entities themselves. These can be classified similarly to entities.

- A *primary entity relation* is an invariant used in entity generation, and so
  is not malleable (save for the number of entities being related).

  In the Google classroom example, this is:
  - every course has exactly one teacher
  - every assignment has exactly one course (enforced by the entity model)
  - a student-assignment pair corresponds to at most one grade
  - a grade corresponds to exactly one student-assignment pair (enforced by the
    entity model)

- A *secondary entity relation* is a one-to-many relation, with the upper bound
  on the cardinality of "many" set by a command line argument.

  In the Google classroom example, this is:
  - each teacher, TA, and student is in at least one course (upper bounds are
    set by `--max-teacher-courseload`, `--max-ta-courseload`,
    `--max-student-courseload`)

- Finally, a *tertiary entity relation* is a relation directly influenced by one
  or more primary or secondary entity relations, and is thus only indirectly
  influenced by the user (direct influence by the user could lead to
  over-constraining or unsatisfiability). These are typically duals of secondary
  entity relations, and are otherwise difficult to characterize.

  In the Google classroom example:
  - there is no restriction on the number of TAs per course or an upper bound on
    the number of students per course.
    - Maximums for either could lead failure to associate some students or TAs
      with courses, which would require backtracking and culling (making these
      primary entity types less directly influenced by the user);
    - A minimum number of TAs for course could conflict with the upper bound on
      TA courseloads.


### Invocation

Currently only executable through stack (will be improved soon).

Run

    stack exec cav2025-cedar-restrict-gclassroom-exe -- --help

for usage instructions. For example, for the default GClassroom case study,

    stack exec cav2025-cedar-restrict-gclassroom-exe -- gc


## Setup

TODO

I suggest using GHCUp to install the Haskell toolchain. Then, run `stack update`
(anywhere) to update Stack's repository cache, then (in project root directory)
`stack build`.

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
