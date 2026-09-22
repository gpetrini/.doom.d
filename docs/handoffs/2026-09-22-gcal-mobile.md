# Handoff — eventos do gcal no Engage, org-gcal nesta máquina, conflitos e Orgzly

**Data:** 2026-09-22
**Repos:** `~/.config/doom` (branch `desktop`), `~/Dropbox/GTD` (branch `main`)
**Máquina:** `gpetrini` — é a **segunda** máquina do handoff de 2026-09-21, não a primeira (`pop-os`).
Continuação de `2026-09-21-org-gcal.md`; leia aquele antes deste.

## Decisão do dia: eventos do Google deixam de ser itens do org-gtd

O bloco "Missed events" (`(type . calendar) (when . past)`) listava permanentemente todo
evento passado do Google.
Causa: `my/gcal-mark-as-gtd-calendar` carimbava `ORG_GTD: Calendar` em cada evento buscado,
e o bloco só exclui itens concluídos, enquanto um evento do Google não tem palavra-chave
TODO alguma e nunca vira `DONE` ou `KILL`.
Eram 94 eventos passados entre os 421 buscados, em regime estacionário limitado a
`org-gcal-up-days` (30) pelo auto-arquivamento do org-gcal.

Duas saídas foram postas; a escolhida foi **B**, que remove o carimbo em vez de filtrar
bloco a bloco:

- `my/gcal-mark-as-gtd-calendar` e seu `add-hook` foram removidos de `config.org`.
- Nova `my/gtd-skip-unless-day-item`: mantém entradas `Calendar`/`Habit` do org-gtd **ou**
  qualquer entrada com a propriedade `org-gcal-managed`, e descarta as concluídas.
- O bloco do dia deixou de ser `(block-type . calendar-day)` e passou a `native`, um bloco
  `agenda` usando aquela função de skip.
- Varredura única em `~/Dropbox/GTD/gcal/`: 842 linhas (`ORG_GTD` e `ORG_GTD_TIMESTAMP`)
  removidas dos 421 eventos, sem tocar em nada mais (conferido com `diff`).
  Backup da sessão em `scratchpad/gcal-backup-20260922-095037/`.
- A prosa da seção `** Gcal sync` registra a reversão, o motivo e a atribuição correta do
  efeito colateral de arquivamento (é `my-gtd-archive-killed-items`, que casa pela
  palavra-chave `KILL` sobre todos os arquivos da agenda, não o carimbo).

Verificado em Emacs `-Q --batch` com org puro: o evento do Google entra no bloco do dia pelo
timestamp ativo dentro do drawer `:org-gcal:`, que `org-agenda-get-timestamps` varre como
qualquer outro, sem depender de `ORG_GTD_TIMESTAMP`; o evento `KILL` e uma ação comum com
timestamp de hoje ficam de fora.

## #5 — org-gcal nesta máquina

Estado encontrado: nenhum `oauth2-auto.plist`, nenhum `~/.gnupg/gpg-agent.conf`, e uma única
chave secreta rsa4096 `CBC079AE0F7AE015`, não a ed25519 `D4498E06…` da primeira máquina.
Os 421 eventos tinham chegado pelo Dropbox, não por fetch local.

Feito:

- `~/.authinfo.gpg` (que já existia, de 2 de maio, com `smtp.gmail.com`, `github.com` e
  `api.github.com`) ganhou as onze linhas do gcal, preservando as anteriores.
  Oito dos dez IDs de agenda foram recuperados da propriedade `:calendar-id:` dos eventos já
  sincronizados, um ID distinto por arquivo; `conferences` e `ysi` vieram de fora porque seus
  arquivos estão vazios.
- Primeira linha do arquivo: `# -*- epa-file-encrypt-to: ("gpetrinidasilveira@gmail.com") -*-`,
  comentário no formato netrc, que fixa o destinatário e evita o prompt de chave a cada
  gravação.
- `plstore-encrypt-to` resolve para `CBC079AE0F7AE015`, e o gpg encaminha para a subchave de
  encriptação `A966BDA3E3BDF34D`; o erro "Unusable public key" do runbook não se aplica aqui.
- `gpg-agent.conf` e as autorizações de navegador foram feitas pelo usuário.

Pendência conhecida, não corrigida: `my/gcal-enabled-p` testa apenas
`(file-exists-p "~/.authinfo.gpg")`, o que é verdadeiro em qualquer máquina com authinfo, com
ou sem credenciais de gcal.
Um portão melhor não pode consultar o auth-source no startup, sob pena de descriptografar o
arquivo a cada inicialização, que é justamente o que o desenho evita.

## #7 — cópias de conflito

Medido: dos 291 arquivos de `~/Dropbox/GTD`, 276 estão dentro de `.git` (4,9 MB de 5,8 MB), e
**as 15 cópias de conflito estavam todas dentro de `.git`**, nenhuma entre os doze `.org`.
O repositório tem remoto no GitHub (`gpetrini/GTD`) e estava idêntico a `origin/main`,
portanto a história não depende do sync de arquivos.

`git fsck` acusava `bad ref` por causa dessas cópias; as quinze foram movidas para
`scratchpad/gtd-git-conflitos/` (não apagadas) e o fsck ficou limpo.
Criado `~/Dropbox/GTD/.stignore` com `.git` e `.stversions`, inerte enquanto o Syncthing não
roda nesta máquina.

Isto é paliativo: enquanto `.git` estiver dentro da pasta sincronizada, as cópias voltam.
A decisão de transporte (#8/#9) foi adiada pelo usuário com a opção "só limpar agora".
Dados para retomá-la: existe `.stfolder`, logo a pasta está declarada também no Syncthing, e
o `.dropboxignore` tem semântica perigosa, pois remove o item da nuvem e das outras máquinas.

## #6 — Orgzly Revived

App instalado pelo **Google Play**, versão **1.23.0**; é a única compilação com backend
Dropbox, ausente por construção no F-Droid e no IzzyOnDroid.

Sintoma: timeout de sincronização, nenhum caderno sincronizado, sem outra mensagem.

Volume descartado por medição: o Dropbox inteiro tem 465 arquivos e 207 MB.

Teste de isolamento: `~/Dropbox/orgzly-teste/teste.org`, com repositório apontando para
`/orgzly-teste`, **sincroniza normalmente**; o mesmo app apontando para `/GTD` **falha**.
Logo o defeito está na forma da pasta `/GTD`, não na autorização nem na rede.

Confirmação posterior, ainda na mesma sessão: desligando "incluir subpastas" no app, o
repositório `/GTD` passou a sincronizar.
Isso sustenta a hipótese da subárvore `.git`, com 276 entradas e nomes com apóstrofo e
parênteses vindos das cópias de conflito, já que a opção desligada impede o app de descer em
`.git/` e em `gcal/`.

Mas essa configuração tem um efeito que o #6 proíbe: com o repositório em `/GTD` e as
subpastas desligadas, o app passa a enxergar os dois `.org` do primeiro nível, `inbox.org` e
**`org-gtd-tasks.org`**, e o Orgzly reescreve o arquivo inteiro que toca.
Os arquivos de `gcal/` ficam de fora, o que é o desejado: no celular os eventos são lidos no
próprio Google Agenda.
A correção é apontar o repositório para uma subpasta dedicada, e não para `/GTD`.

A decisão de #6 continua a mesma: o celular vê um único arquivo, numa subpasta dedicada.
A escolha entre `mobile.org` exclusivo e `inbox.org` direto continua aberta.

## Retomar por aqui

1. Apontar o repositório do Orgzly para uma subpasta dedicada, em vez de `/GTD` com as
   subpastas desligadas, que hoje expõe `org-gtd-tasks.org` ao app.
2. Fechar #6 escolhendo entre `mobile.org` e `inbox.org`, criar a subpasta dedicada e apontar
   o Orgzly para ela.
3. Apagar `~/Dropbox/orgzly-teste/` quando o diagnóstico terminar.
4. Decidir #8/#9 (transporte), que é o que impede as cópias de conflito de voltarem.
5. Opcional: trocar o portão `my/gcal-enabled-p` por um marcador por máquina.

## Não commitado de propósito

`packages.el` em `~/.config/doom` traz `;;(package! nov)`, edição do usuário anterior a esta
sessão, deixada como está.
