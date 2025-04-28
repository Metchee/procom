#!/bin/bash
# Script de test complet pour myPandoc

# Couleurs pour une meilleure lisibilité
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Compteurs pour le suivi des tests
TOTAL_TESTS=0
PASSED_TESTS=0

# Fonction pour créer le répertoire de test
create_test_dir() {
    echo -e "${YELLOW}Création du répertoire de test...${NC}"
    mkdir -p test_mypandoc
    cd test_mypandoc
}

# Fonction pour créer les fichiers de test
create_test_files() {
    echo -e "${YELLOW}Création des fichiers de test...${NC}"
    
    # Fichier XML simple
    cat > simple.xml << 'EOF'
<document>
  <header title="Document Simple" author="Testeur" date="2025-04-28"></header>
  <body>
    <paragraph>Ceci est un document XML simple.</paragraph>
    <paragraph>Il contient du <bold>texte en gras</bold> et du <italic>texte en italique</italic>.</paragraph>
  </body>
</document>
EOF

    # Fichier XML complexe
    cat > complex.xml << 'EOF'
<document>
  <header title="Document Complexe" author="Testeur" date="2025-04-28"></header>
  <body>
    <paragraph>Ce document contient plusieurs éléments.</paragraph>
    <section title="Une section">
      <paragraph>Contenu de la section avec <bold>formatage</bold>.</paragraph>
      <list>
        <item>Premier élément</item>
        <item>Deuxième élément</item>
      </list>
      <codeblock>
function test() {
  console.log("Test");
}
      </codeblock>
    </section>
    <paragraph>Voici un <link text="lien" url="https://example.com"></link>.</paragraph>
    <paragraph>Et une <image alt="image" url="image.png"></image>.</paragraph>
  </body>
</document>
EOF

    # Fichier JSON simple
    cat > simple.json << 'EOF'
{
  "header": {
    "title": "Document Simple JSON",
    "author": "Testeur",
    "date": "2025-04-28"
  },
  "body": [
    {
      "type": "paragraph",
      "content": [
        "Ceci est un document JSON simple."
      ]
    },
    {
      "type": "paragraph",
      "content": [
        "Il contient du ",
        {
          "type": "bold",
          "content": "texte en gras"
        },
        " et du ",
        {
          "type": "italic",
          "content": "texte en italique"
        },
        "."
      ]
    }
  ]
}
EOF

    # Fichier Markdown simple
    cat > simple.md << 'EOF'
---
title: Document Simple Markdown
author: Testeur
date: 2025-04-28
---

Ceci est un document Markdown simple.

Il contient du **texte en gras** et du *texte en italique*.
EOF

    # Fichier Markdown complexe
    cat > complex.md << 'EOF'
---
title: Document Complexe Markdown
author: Testeur
date: 2025-04-28
---

Ce document contient plusieurs éléments.

# Une section

Contenu de la section avec **formatage**.

- Premier élément
- Deuxième élément

```
function test() {
  console.log("Test");
}
```

Voici un [lien](https://example.com).

Et une ![image](image.png).
EOF

    # Fichier XML invalide
    cat > invalid.xml << 'EOF'
<document>
  <header title="Document invalide">
  <body>
    <paragraph>Ce document XML est mal formé (il manque une balise fermante).</paragraph>
  </body>
</document>
EOF
}

# Fonction pour exécuter un test de conversion
run_conversion_test() {
    local input=$1
    local format=$2
    local output=$3
    local input_format=$4
    
    ((TOTAL_TESTS++))
    
    input_format_param=""
    if [ ! -z "$input_format" ]; then
        input_format_param="-e $input_format"
    fi
    
    echo -n "Test $TOTAL_TESTS: Conversion de $input vers $format... "
    
    # Exécution de la commande avec redirection de stderr
    if ../mypandoc -i $input -f $format -o $output $input_format_param > /dev/null 2> test_stderr.log; then
        # Vérifier que le fichier de sortie existe et n'est pas vide
        if [ -s "$output" ]; then
            echo -e "${GREEN}OK${NC}"
            ((PASSED_TESTS++))
        else
            echo -e "${RED}ÉCHEC${NC} (fichier de sortie vide)"
            echo -e "${BLUE}stderr:${NC} $(cat test_stderr.log)"
        fi
    else
        echo -e "${RED}ÉCHEC${NC} (conversion a échoué)"
        echo -e "${BLUE}stderr:${NC} $(cat test_stderr.log)"
    fi
}

# Fonction pour exécuter un test d'erreur
run_error_test() {
    local input=$1
    local format=$2
    local description=$3
    
    ((TOTAL_TESTS++))
    
    echo -n "Test $TOTAL_TESTS: $description... "
    
    # On s'attend à ce que cette commande échoue
    if ! ../mypandoc -i $input -f $format > /dev/null 2> test_stderr.log; then
        echo -e "${GREEN}OK${NC} (erreur détectée comme prévu)"
        ((PASSED_TESTS++))
    else
        echo -e "${RED}ÉCHEC${NC} (erreur non détectée)"
    fi
}

# Fonction pour exécuter tous les tests
run_all_tests() {
    echo -e "${YELLOW}Exécution des tests de conversion...${NC}"
    
    # Tests XML → JSON, Markdown
    run_conversion_test "simple.xml" "json" "simple_xml_to_json.json"
    run_conversion_test "simple.xml" "markdown" "simple_xml_to_md.md"
    run_conversion_test "complex.xml" "json" "complex_xml_to_json.json"
    run_conversion_test "complex.xml" "markdown" "complex_xml_to_md.md"
    
    # Tests JSON → XML, Markdown
    run_conversion_test "simple.json" "xml" "simple_json_to_xml.xml"
    run_conversion_test "simple.json" "markdown" "simple_json_to_md.md"
    
    # Tests Markdown → XML, JSON
    run_conversion_test "simple.md" "xml" "simple_md_to_xml.xml"
    run_conversion_test "simple.md" "json" "simple_md_to_json.json"
    run_conversion_test "complex.md" "xml" "complex_md_to_xml.xml"
    run_conversion_test "complex.md" "json" "complex_md_to_json.json"
    
    # Tests avec format d'entrée explicite
    run_conversion_test "simple.xml" "json" "explicit_xml_to_json.json" "xml"
    run_conversion_test "simple.json" "markdown" "explicit_json_to_md.md" "json"
    run_conversion_test "simple.md" "xml" "explicit_md_to_xml.xml" "markdown"
    
    # Tests d'erreur
    run_error_test "fichier_inexistant.xml" "json" "Fichier inexistant"
    run_error_test "invalid.xml" "json" "Document XML invalide"
    run_error_test "simple.xml" "html" "Format de sortie non supporté"
    
    # Affichage du résumé
    echo -e "${YELLOW}Résumé: ${PASSED_TESTS}/${TOTAL_TESTS} tests réussis${NC}"
    
    if [ $PASSED_TESTS -eq $TOTAL_TESTS ]; then
        echo -e "${GREEN}Tous les tests ont réussi !${NC}"
    else
        echo -e "${RED}Certains tests ont échoué.${NC}"
    fi
}

# Fonction pour afficher les détails des résultats
show_results() {
    echo -e "${YELLOW}Détails des résultats :${NC}"
    
    # Liste tous les fichiers générés
    for file in *_to_*.*; do
        if [ -f "$file" ]; then
            size=$(wc -c < "$file")
            echo -e "${BLUE}$file${NC} - Taille: $size octets"
            
            # Pour les petits fichiers, afficher le contenu
            if [ $size -lt 500 ]; then
                echo "---"
                cat "$file"
                echo "---"
            else
                echo "Fichier trop grand pour être affiché"
            fi
            
            echo ""
        fi
    done
}

# Fonction pour nettoyer le répertoire de test
cleanup() {
    echo -e "${YELLOW}Nettoyage du répertoire de test...${NC}"
    cd ..
    rm -rf test_mypandoc
}

# Fonction principale
main() {
    create_test_dir
    create_test_files
    run_all_tests
    
    # Afficher les détails des résultats si demandé
    if [ "$1" == "--show-results" ]; then
        show_results
    fi
    
    cleanup
}

# Exécution du script
main "$@"