# ORG_GTD_TIMESTAMP vs SCHEDULED/DEADLINE em itens sincronizados

Pesquisa para gpetrini/.doom.d#3, ligada ao mapa #1 (integração org-todoist + org-gtd).

Código fonte consultado em `~/.config/emacs/.local/straight/repos/org-gtd.el/` (org-gtd v4).

## 1. As views do org-gtd leem ORG_GTD_TIMESTAMP ou caem de volta para SCHEDULED/DEADLINE?

Não há fallback. As views leem exclusivamente a property `ORG_GTD_TIMESTAMP`.

- `org-gtd-types.el:51,61,73` define o mapeamento semântico `:when -> :org-property "ORG_GTD_TIMESTAMP"` para os tipos `delegated`, `calendar` e `tickler`. Nenhum desses três tipos declara SCHEDULED/DEADLINE como fonte alternativa.
  - Único tipo que usa SCHEDULED nativamente é `habit` (`org-gtd-types.el:99`, `:org-property "SCHEDULED"`), não Calendar/Tickler/Delegated.
- `org-gtd-view-language.el:825` (`org-gtd-view-lang--build-skip-function`) resolve o filtro `when` chamando `org-gtd-type-property type-filter :when`, que devolve o nome literal da property (`org-gtd-types.el:319-324`, `org-gtd-type-property`).
- Esse nome de property é passado a `org-gtd-pred--property-ts<` / `-ts>` / `-ts=` (`org-gtd-skip.el:446-479`), que leem o valor via `(org-entry-get (point) property)` (`org-gtd-skip.el:451,461,472`) — ou seja, sempre a property `ORG_GTD_TIMESTAMP`, nunca `SCHEDULED`/`DEADLINE` do headline.
- Quando a property está ausente ou inválida, o predicado retorna `nil` (`org-gtd-skip.el:451,461,472`, guardas `when-let`), e o item simplesmente não aparece nas views `past`/`today`/`future`. A própria documentação do módulo trata isso como caso separado: tipos `stuck-calendar`/`stuck-tickler` existem justamente para capturar "Calendar/Tickler items missing timestamp" (`org-gtd-view-language.el:53-54,196-199`).

Consequência direta: um item Calendar/Tickler/Delegated sincronizado pelo org-todoist, que grava a data em `SCHEDULED`/`DEADLINE` do headline em vez de `ORG_GTD_TIMESTAMP`, é invisível para `org-gtd-view-show` e para `my/gtd-daily-view` (config.org:2025) nos blocos "Missed events", "Missed check-in", "Due today" etc. Ele cairia, na melhor das hipóteses, nas views `stuck-calendar`/`stuck-tickler` como item "sem timestamp", mesmo tendo uma data válida em SCHEDULED/DEADLINE.

## 2. Existe sincronização automática embutida ORG_GTD_TIMESTAMP <-> SCHEDULED/DEADLINE no org-gtd v4?

Não. Busca por `org-schedule`, `org-deadline`, `SCHEDULED`, `DEADLINE` em `org-gtd-hooks.el`, `org-gtd-projects.el`, `org-gtd-next-action.el` e `org-gtd-core.el` não retornou nenhuma ocorrência. O único ponto do código que escreve `ORG_GTD_TIMESTAMP` é o fluxo de organização nativo (`org-gtd-core.el:87-92`, prompt interativo `org-gtd-prompt-for-active-date` + `org-entry-put`), acionado manualmente (ou via advice `my-gtd-smart-schedule` em config.org:1996-2007). Não há hook, advice interno ou watcher que espelhe SCHEDULED/DEADLINE para ORG_GTD_TIMESTAMP (ou vice-versa) após uma edição externa do headline.

## Conclusão

É necessário um hook adicional. Sem ele, itens Calendar/Tickler/Delegated sincronizados via org-todoist (que escreve SCHEDULED/DEADLINE) nunca aparecerão corretamente nas agendas nativas do org-gtd — vão parecer "sem timestamp" (stuck) mesmo com data válida.

Hook proposto: após cada sync do org-todoist (ex. advice/hook `:after` em torno da função de sync que atualiza SCHEDULED/DEADLINE, ou um `org-after-todo-state-change-hook`/timer periódico sobre a árvore Todoist), copiar `SCHEDULED`/`DEADLINE` do headline para a property `ORG_GTD_TIMESTAMP` quando `ORG_GTD` for `Tickler`, `Calendar` ou `Delegated`. Escopo de implementação fica para a issue de execução (fora do escopo desta pesquisa).
