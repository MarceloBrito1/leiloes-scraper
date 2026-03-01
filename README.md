# Leiloes Scraper (Zuk + MegaLeiloes + Leeilon)

Projeto separado para raspagem de oportunidades de leiloes imobiliarios.

## O que faz
- Suporta `portalzuk.com.br` com paginação por `carregar mais`.
- Suporta `megaleiloes.com.br` com paginação por `?pagina=`.
- Mantem fallback para `leeilon.com.br` (somente conteudo estatico).

## Requisitos
- R (>= 4.2)
- Pacotes R: `httr`, `xml2`, `jsonlite`, `stringr`

## Uso direto
```bash
Rscript scripts/scrape_leiloes.R --url "https://www.portalzuk.com.br/leilao-de-imoveis" --site zuk --max-pages 5 --out "resultado.csv"
```

## Execucao com historico local
Use o wrapper PowerShell para salvar cada execucao em uma pasta com timestamp:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\run_and_archive.ps1 -Url "https://www.megaleiloes.com.br/imoveis?pagina=1" -Site megaleiloes -MaxPages 3
```

Isso cria:
- `local_progress/YYYYMMDD_HHMMSS/resultado.csv` (ou `.json`)
- `local_progress/YYYYMMDD_HHMMSS/execucao.log`
- `local_progress/YYYYMMDD_HHMMSS/params.json`
