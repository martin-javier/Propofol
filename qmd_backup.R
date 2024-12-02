---
  title: |
  <div class="title-large">Einfluss von Propofol</div>
    <div class="title-small">auf Verweildauer und Sterblichkeit auf der Intensivstation</div>
      subtitle: "Prof. Dr. Wolfgang Hartl"
    author: "Cong , Martin, Ramn, Iman, Lukas "
    institute: "Dr. Andreas Bender, Dr. Mona Niethammer"
    date: "2024-12-05"
    format:
      revealjs:
      css: customstyle.css
    embed-resources: true
    slide-number: c
    logo: "icons/LMU_Logo.svg"
    editor: visual
    ---
      
      ###  Agenda
      
      ::: incremental
    1. **Einleitung**
      - Überblick über die Studie und Ziele.
    - Bedeutung der Forschung.
    
    2. **Datenüberblick**
      - Beschreibung der Datensätze.
    - Einschluss-/Ausschlusskriterien.
    - Verwendete Confounder.
    
    3. **Methodik**
      - Vorverarbeitung und Zusammenführung der Datensätze.
    - Verwendete statistische Modelle.
    
    4. **Ergebnisse**
      - Wichtige Diagramme und Tabellen.
    - Subgruppenanalysen.
    
    5. **Fazit**
      - Zusammenfassung der Ergebnisse.
    - Einschränkungen und zukünftige Arbeiten.
    
    :::
      
      # Datenüberblick
      
      Bereits gesäuberter Datensatz von Andreas Bender:
      
      - **Datengröße**:
      - Ursprünglich: ca. 182.000 Beobachtungen und 51 Variablen => 17.000 Patienten mit jeweils 11 Beobachtungstagen.
    - Aktuell: ca. 12.000 Beobachtungen und 27 Variablen (12.000 Patienten).
    
    - **Patienten von Interesse**:
      - Alter von mindestens 18 Jahren.
    - BMI über 13 kg/m².
    - Aufenthaltsdauer auf der Intensivstation von mindestens 7 Tagen.
    - Nur Propofol-Einnahme innerhalb der ersten 7 Tage nach Aufnahme wird analysiert.
    
    ---
      
      ### Verwendete Confounder
      
      - **Alter**
      - **BMI**
      - **APACHE-II-Score**
      - **Zufälliger Effekt für Intensivstation**
      - **Geschlecht**
      - **Jahr der Behandlung (kategorial)**
      - **admission category (kategorial)**
      - **leading admission diagnosis (kategorial)**
      - **Anzahl der Tage (0-7) mit mechanischer Beatmung**
      - **Tage mit oral intake (Nahrungsaufnahme)**
      - **Tage mit parenteral nutrition**
      - **Tage mit protein intake**
      
      ---
      
      # Methodik
      
      ### Datenvorverarbeitung
      
      1. Zusammenführung der Datensätze.
    2. Umgang mit fehlenden Werten.
    3. Berechnung wichtiger Metriken:
      - Kumulative Propofol-Dosis.
    - Tage mit Propofol-Einnahme.
    
    ### Statistische Modelle
    
    #### Cox-Proportional-Hazards-Modell
    
    Erklärung der Verwendung für:
      - Tod und Entlassung als konkurrierende Risiken.
    - Anpassungen für Störvariablen:
      - Alter, BMI, Apache II-Score usw.
    
    #### Penalized Regression
    
    Beschreibung der Nutzung von Splines und Lag-Lead-Funktionen.
    
    ---
      
      # Ergebnisse
      
      ### Überlebensanalyse