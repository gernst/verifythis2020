# Notizen
### Zur History: (Großteils implementiert)

- **Client erweitern**:
  - Liste von Entscheidungen -> So wie Messages : Alle Entscheidungen "high level"
    Paare von Fingerprint und Identity
    Global
  
  **2** wichtige Arten von Nachrichten
    1. Entscheidung: Fingerprint verknüpft mit Menge an Identities
    2. Revoke von Verbindungen Fingerprint und Identity
 
  **Idee**: 
    - History als Ablaufplan für Clients 
    - History wird von ScalaCheck generiert.
 
  2 Arten die History zu interpretieren:
  -> Actor die die Events ausführen.
  -> Checker, der nur die History verarbeitet.
 
  **Angreifer**:
    - Checks über "History" checken, ob bestimmte Sichtbarkeiten okay sind. 
    - Angreifer vlt kein direkter Zugriff, stattdessen auch Messages in der Historie.
        -> Prüfen, ob Historie so erlaubt ist.
 
  **TODO**: Egal über Fingerprint/KeyId/Email -> Immer Key filtern
 
  **Funktionale Eigenschaften**: Key ist selber bei Upload und download
 
 
  Gegeben fingerprint -> Public identities auf basis der History zurückgeben.
 
 
  Aufschreiben.
  Linearisierbarkeit
 
  
---
### Zu den Actors: 
   **Neue Idee**:
   - Keine Actors -> Stattdessen einfach eine große Routine, die die History abarbeitet und mit dem Server kommuniziert.
   - Vlt auch mit ungültigen Anfragen.
 
 
 Durch Histoy durchlaufen und sich Verify und Revoke anschauen.
 
 Map durchlaufen und für fingerprint anfragen an den Server schicken -> Response dann mit Map eintrag abgleichen