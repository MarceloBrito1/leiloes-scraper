# Leiloes Scraper (Zuk + MegaLeiloes + Leeilon)

Projeto separado para raspagem de oportunidades de leiloes imobiliarios.

## O que faz
- Suporta `portalzuk.com.br` com paginacao por `carregar mais`.
- Suporta `megaleiloes.com.br` com paginacao por `?pagina=`.
- Suporta `leeilon.com.br` via endpoint dinamico oficial (server action), com paginacao por `page`.
- Percorre todas as paginas por padrao (`--max-pages all`).
- Faz sanitizacao automatica da URL de entrada (remove wrappers/redirecionadores e entra direto na rota de busca), reduzindo impacto de popups/anuncios de entrada.
- Possui retentativa automatica com backoff para respostas `HTTP 429`/`5xx`, reduzindo falhas por bloqueio de taxa.

## Filtros de relatorio
Permite filtrar por:
- modalidade (`--auction-type any|judicial|extrajudicial`)
- fase atual (`--current-round any|1|2|ended`)
- janela de datas (`--date-from`, `--date-to`)
- campo de data (`--date-field next|first|second|any`)

## Filtros nativos dos sites
- MegaLeiloes: quando `--auction-type` e informado, o scraper usa rota nativa:
  - judicial -> `https://www.megaleiloes.com.br/leiloes-judiciais`
  - extrajudicial -> `https://www.megaleiloes.com.br/leiloes-extrajudiciais`
- Zuk: o scraper tenta aplicar `comitente_judicial` na URL e reforca o filtro no pos-processamento.
- Leeilon: quando `--auction-type` e informado, o scraper aplica `modalidades=Judicial|Extrajudicial`.

## Requisitos
- R (>= 4.2)
- Pacotes R: `httr`, `xml2`, `jsonlite`, `stringr`
- Python 3.10+ (para painel web)

## Painel web (pagina local)
Para usar como pagina da internet (rodando localmente):

```powershell
pip install -r requirements-web.txt
powershell -ExecutionPolicy Bypass -File .\scripts\start_web.ps1
```

Depois abra: `http://127.0.0.1:8787`

No painel voce consegue:
- informar URL/site e filtros
- informar usuario/senha (opcional) antes da raspagem
- executar raspagem e ver preview em tabela
- baixar `resultado`, `execucao.log` e `params.json`
- reabrir execucoes anteriores salvas em `local_progress/`

## Uso direto
```bash
Rscript scripts/scrape_leiloes.R --url "https://www.portalzuk.com.br/leilao-de-imoveis" --site zuk --out "resultado.csv"
```

Por padrao, o scraper percorre todas as paginas disponiveis.
Use `--max-pages N` apenas se quiser limitar.

### Login por usuario/senha
O scraper suporta sessao autenticada por cookies para Zuk, MegaLeiloes e Leeilon.

No uso direto, informe credenciais por variaveis de ambiente:

```powershell
$env:LEILOES_LOGIN_USER="seu_usuario"
$env:LEILOES_LOGIN_PASS="sua_senha"
Rscript scripts/scrape_leiloes.R --url "https://www.leeilon.com.br/busca-leilao?page=1" --site leeilon --out "leeilon.csv"
```

Opcionalmente, voce pode usar variaveis por site:
- `LEILOES_ZUK_USER` / `LEILOES_ZUK_PASS`
- `LEILOES_MEGALEILOES_USER` / `LEILOES_MEGALEILOES_PASS`
- `LEILOES_LEEILON_USER` / `LEILOES_LEEILON_PASS`

### Exemplo com filtros
```bash
Rscript scripts/scrape_leiloes.R \
  --url "https://www.megaleiloes.com.br/imoveis?pagina=1" \
  --site megaleiloes \
  --auction-type judicial \
  --current-round 2 \
  --date-from 2026-03-01 \
  --date-to 2026-03-31 \
  --date-field next \
  --out "relatorio_filtrado.csv"
```

### Exemplo Leeilon
```bash
Rscript scripts/scrape_leiloes.R \
  --url "https://www.leeilon.com.br/busca-leilao?estado=S%C3%A3o+Paulo&categorias=Apartamento,Casa&modalidades=Judicial&page=1" \
  --site leeilon \
  --max-pages all \
  --out "leeilon.csv"
```

## Execucao com historico local
Use o wrapper PowerShell para salvar cada execucao em uma pasta com timestamp:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\run_and_archive.ps1 -Url "https://www.megaleiloes.com.br/imoveis?pagina=1" -Site megaleiloes
```

O wrapper pergunta usuario/senha antes de iniciar (senha oculta) e injeta as credenciais na execucao sem gravar a senha no comando/log.

Para execucao sem prompt de login:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\run_and_archive.ps1 -Url "https://www.megaleiloes.com.br/imoveis?pagina=1" -Site megaleiloes -NoLoginPrompt
```

Com filtros:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\run_and_archive.ps1 `
  -Url "https://www.megaleiloes.com.br/imoveis?pagina=1" `
  -Site megaleiloes `
  -AuctionType judicial `
  -CurrentRound 2 `
  -DateFrom 2026-03-01 `
  -DateTo 2026-03-31 `
  -DateField next
```

Isso cria:
- `local_progress/YYYYMMDD_HHMMSS/resultado.csv` (ou `.json`)
- `local_progress/YYYYMMDD_HHMMSS/execucao.log`
- `local_progress/YYYYMMDD_HHMMSS/params.json`
