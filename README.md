# 🏊 Zwemsterdam

**Alle zwemtijden van Amsterdamse zwembaden op één plek.**

Zwemsterdam is een web-app die automatisch zwemtijden verzamelt van alle openbare zwembaden in Amsterdam, zodat je in één overzicht kunt zien wanneer en waar je kunt zwemmen.

🌐 **Live:** [zwemsterdam.nl](https://zwemsterdam.nl)

---

## ✨ Features

- **📊 Tijdlijn weergave** – Horizontale tijdbalk per zwembad, ideaal voor een snel overzicht
- **📅 Kalender weergave** – Klassieke rooster-weergave met sessions die hun werkelijke duur tonen
- **📋 Lijst weergave** – Alle sessies op een rij, perfect voor mobiel
- **🔍 Slim filteren** – Filter op dag, activiteit (banenzwemmen, recreatief, etc.) en zwembad
- **⏰ Live "Nu" indicator** – Zie direct welke sessies nu open zijn
- **📱 Responsive** – Werkt perfect op telefoon, tablet en desktop
- **🔗 Directe links** – Klik door naar de officiële website van elk zwembad

---

## 🏊‍♀️ Ondersteunde Zwembaden

### Gemeente Amsterdam (via Amsterdam API)
- [Zuiderbad](https://www.amsterdam.nl/zuiderbad/)
- [Noorderparkbad](https://www.amsterdam.nl/noorderparkbad/)
- [De Mirandabad](https://www.amsterdam.nl/demirandabad/rooster/)
- [Flevoparkbad](https://www.amsterdam.nl/flevoparkbad/)
- [Brediusbad](https://www.amsterdam.nl/brediusbad/)

### Overige Zwembaden
- [Het Marnix](https://hetmarnix.nl/)
- [Sportfondsenbad Oost](https://www.sportfondsenbadamsterdamoost.nl/)
- [Sportplaza Mercator](https://www.sportplazamercator.nl/)
- [De Meerkamp (Amstelveen)](https://amstelveensport.nl/zwembad-de-meerkamp/)

### Nog niet ondersteund
- **Sloterparkbad** & **Bijlmer Sportcentrum** - Beheerd door Optisport, vereist browser automation (Cloudflare)

---

## 🛠️ Technologie

### Frontend
- **React 19** met TypeScript
- **Vite** als build tool
- **TailwindCSS** + **DaisyUI** voor styling
- **Lucide React** voor icons

### Backend / Data
- **R** voor data scraping en verwerking
- **GitHub Actions** voor automatische dagelijkse updates
- Geen database nodig – statische JSON data

---

## 🚀 Zelf draaien

### Vereisten
- Node.js 18+
- R 4.0+ (alleen voor data updates)

### Frontend development

```bash
cd frontend
npm install
npm run dev
```

De app draait nu op `http://localhost:5173`

### Data updaten

```bash
# Installeer R packages (eenmalig)
Rscript -e "install.packages(c('tidyverse', 'jsonlite', 'httr', 'rvest'))"

# Run data collection
Rscript fin.R
```

### Production build

```bash
cd frontend
npm run build
```

Output komt in `frontend/dist/`

---

## 📁 Project Structuur

```
zwemsterdam/
├── frontend/               # React frontend
│   ├── src/
│   │   ├── App.tsx        # Main application
│   │   ├── index.css      # Tailwind + custom styles
│   │   └── main.tsx       # Entry point
│   ├── public/
│   │   ├── data.json      # Swimming session data
│   │   └── metadata.json  # Last update info
│   └── package.json
├── fin.R                   # Main data collection script
├── utils.R                 # Scraping utilities
├── data/                   # Raw data storage
└── .github/workflows/      # Automated updates
```

---

## ⚠️ Disclaimer

**Zwemsterdam is een onofficieel hulpmiddel.** De data wordt automatisch verzameld van openbare bronnen, maar kan fouten bevatten of verouderd zijn. 

**Controleer altijd de officiële website van het zwembad** voor de meest actuele zwemtijden voordat je gaat zwemmen.

---

## 👨‍💻 Gemaakt door

**Fabio Votta** ([@favstats](https://github.com/favstats))

---

## ☕ Support

Vind je Zwemsterdam handig? Overweeg een kleine donatie!

<a href="https://www.buymeacoffee.com/favstats" rel="nofollow">
    <img src="https://img.buymeacoffee.com/button-api/?text=Buy%20me%20a%20coffee&emoji=&slug=favstats&button_colour=FFDD00&font_colour=000000&font_family=Arial&outline_colour=000000&coffee_colour=ffffff" alt="Buy Me a Coffee" style="height: 40px; max-width: 100%;">
</a>

---

## 📄 Licentie

MIT License – Vrij te gebruiken, aanpassen en delen.

---

*Veel zwemplezier! 🏊‍♂️💦*
