CREATE TABLE {{sql_name}} (
{{#each fields}}    {{> column}},
{{/each}});