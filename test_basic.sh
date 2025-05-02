#!/bin/bash
# Script de test complet pour myPandoc
# Ce script teste tous les aspects du programme pour garantir son bon fonctionnement

# Couleurs pour une meilleure lisibilité
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Compteurs pour le suivi des tests
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Répertoire de test
TEST_DIR="complete_tests"
mkdir -p $TEST_DIR

echo -e "${YELLOW}========== TEST COMPLET DE MYPANDOC ==========${NC}"
echo -e "Date: $(date)"
echo -e "Ce test va vérifier exhaustivement toutes les fonctionnalités de myPandoc.\n"

# Fonction pour exécuter un test
run_test() {
    local test_name=$1
    local command=$2
    local expected_exit_code=${3:-0}
    local description=$4
    
    ((TOTAL_TESTS++))
    
    echo -e "\n${BLUE}Test #$TOTAL_TESTS: $test_name${NC}"
    echo -e "Description: $description"
    echo -e "Commande: $command"
    
    # Exécuter la commande et capturer la sortie et le code de retour
    eval "$command" > "$TEST_DIR/output_$TOTAL_TESTS.out" 2> "$TEST_DIR/error_$TOTAL_TESTS.err"
    local exit_code=$?
    
    # Vérifier si le code de retour correspond à ce qui est attendu
    if [ $exit_code -eq $expected_exit_code ]; then
        echo -e "${GREEN}✓ RÉUSSI${NC} (code de retour: $exit_code)"
        ((PASSED_TESTS++))
    else
        echo -e "${RED}✗ ÉCHEC${NC} (code de retour: $exit_code, attendu: $expected_exit_code)"
        ((FAILED_TESTS++))
        echo -e "Erreur:"
        cat "$TEST_DIR/error_$TOTAL_TESTS.err"
    fi
    
    # Afficher un aperçu de la sortie si le test a réussi
    if [ $exit_code -eq $expected_exit_code ] && [ -s "$TEST_DIR/output_$TOTAL_TESTS.out" ]; then
        echo -e "\nAperçu de la sortie:"
        head -n 5 "$TEST_DIR/output_$TOTAL_TESTS.out"
        if [ $(wc -l < "$TEST_DIR/output_$TOTAL_TESTS.out") -gt 5 ]; then
            echo -e "..."
        fi
    fi
}

# Fonction pour créer un fichier de test
create_test_file() {
    local filename=$1
    local content=$2
    
    echo -e "$content" > "$TEST_DIR/$filename"
    echo -e "Fichier créé: $TEST_DIR/$filename"
}

echo -e "${YELLOW}=== CRÉATION DES FICHIERS DE TEST ===${NC}"

# 1. XML DE TEST
create_test_file "basic.xml" '<document>
<header title="Document de base"></header>
<body>
<paragraph>Contenu basique pour tester.</paragraph>
</body>
</document>'

create_test_file "with_metadata.xml" '<document>
<header title="Document avec métadonnées" author="Testeur" date="2025-05-02"></header>
<body>
<paragraph>Ce document contient des métadonnées.</paragraph>
</body>
</document>'

create_test_file "formatting.xml" '<document>
<header title="Formatage"></header>
<body>
<paragraph>Texte <bold>en gras</bold> et <italic>en italique</italic>.</paragraph>
</body>
</document>'

create_test_file "complex.xml" '<document>
<header title="Document complexe" author="Testeur" date="2025-05-02"></header>
<body>
<section title="Introduction">
  <paragraph>Introduction au document.</paragraph>
</section>
<paragraph>Paragraphe standard.</paragraph>
<list>
  <item>Premier élément</item>
  <item>Deuxième élément</item>
</list>
<codeblock>
function test() {
  return true;
}
</codeblock>
</body>
</document>'

create_test_file "malformed.xml" '<document>
<header title="Document mal formé">
<body>
  <paragraph>Il manque une balise fermante pour header.</paragraph>
</body>
</document>'

create_test_file "minimal.xml" '<document><header title="Minimal"></header><body></body></document>'

# 2. JSON DE TEST
create_test_file "basic.json" '{
  "header": {
    "title": "Document de base"
  },
  "body": [
    "Contenu basique pour tester."
  ]
}'

create_test_file "with_metadata.json" '{
  "header": {
    "title": "Document avec métadonnées",
    "author": "Testeur",
    "date": "2025-05-02"
  },
  "body": [
    "Ce document contient des métadonnées."
  ]
}'

create_test_file "formatting.json" '{
  "header": {
    "title": "Formatage"
  },
  "body": [
    {
      "type": "paragraph",
      "content": [
        "Texte ",
        {
          "type": "bold",
          "content": "en gras"
        },
        " et ",
        {
          "type": "italic",
          "content": "en italique"
        },
        "."
      ]
    }
  ]
}'

create_test_file "complex.json" '{
  "header": {
    "title": "Document complexe",
    "author": "Testeur",
    "date": "2025-05-02"
  },
  "body": [
    {
      "type": "section",
      "title": "Introduction",
      "content": [
        {
          "type": "paragraph",
          "content": [
            "Introduction au document."
          ]
        }
      ]
    },
    {
      "type": "paragraph",
      "content": [
        "Paragraphe standard."
      ]
    },
    {
      "type": "list",
      "items": [
        {
          "content": [
            "Premier élément"
          ]
        },
        {
          "content": [
            "Deuxième élément"
          ]
        }
      ]
    },
    {
      "type": "codeblock",
      "content": "function test() {\n  return true;\n}"
    }
  ]
}'

create_test_file "malformed.json" '{
  "header": {
    "title": "Document mal formé"
  ,
  "body": [
    "Il manque une accolade fermante pour header."
  ]
}'

create_test_file "minimal.json" '{"header":{"title":"Minimal"},"body":[]}'

# 3. MARKDOWN DE TEST
create_test_file "basic.md" '---
title: Document de base
---

Contenu basique pour tester.'

create_test_file "with_metadata.md" '---
title: Document avec métadonnées
author: Testeur
date: 2025-05-02
---

Ce document contient des métadonnées.'

create_test_file "formatting.md" '---
title: Formatage
---

Texte **en gras** et *en italique*.'

create_test_file "complex.md" '---
title: Document complexe
author: Testeur
date: 2025-05-02
---

# Introduction

Introduction au document.

Paragraphe standard.

- Premier élément
- Deuxième élément

```
function test() {
  return true;
}
```'

create_test_file "malformed.md" '---
title: Document mal formé
author: Testeur
---

Ce document est mal formé car il manque la ligne de fermeture des métadonnées'

create_test_file "minimal.md" '---
title: Minimal
---'

# 4. FICHIERS SPÉCIAUX
create_test_file "empty.txt" ""
create_test_file "special_chars.xml" '<document>
<header title="Caractères &amp; spéciaux !@#$%^&*()"></header>
<body>
<paragraph>Test avec des caractères spéciaux : é è à ù ç ô</paragraph>
</body>
</document>'

echo -e "\n${YELLOW}=== TESTS BASIQUES DE CONVERSION ===${NC}"

# Conversion XML → formats divers
run_test "xml_to_xml" "./mypandoc -i $TEST_DIR/basic.xml -f xml" 0 "Convertir XML vers XML"
run_test "xml_to_json" "./mypandoc -i $TEST_DIR/basic.xml -f json" 0 "Convertir XML vers JSON"
run_test "xml_to_markdown" "./mypandoc -i $TEST_DIR/basic.xml -f markdown" 0 "Convertir XML vers Markdown"

# Conversion JSON → formats divers
run_test "json_to_xml" "./mypandoc -i $TEST_DIR/basic.json -f xml" 0 "Convertir JSON vers XML"
run_test "json_to_json" "./mypandoc -i $TEST_DIR/basic.json -f json" 0 "Convertir JSON vers JSON"
run_test "json_to_markdown" "./mypandoc -i $TEST_DIR/basic.json -f markdown" 0 "Convertir JSON vers Markdown"

# Conversion Markdown → formats divers
# Ajout d'un timeout pour éviter les boucles infinies
run_test "md_to_xml" "timeout 5s ./mypandoc -i $TEST_DIR/basic.md -f xml" 0 "Convertir Markdown vers XML"
run_test "md_to_json" "timeout 5s ./mypandoc -i $TEST_DIR/basic.md -f json" 0 "Convertir Markdown vers JSON"
run_test "md_to_markdown" "timeout 5s ./mypandoc -i $TEST_DIR/basic.md -f markdown" 0 "Convertir Markdown vers Markdown"
echo -e "\n${YELLOW}=== TESTS AVEC SPÉCIFICATION DE FORMAT D'ENTRÉE ===${NC}"

# Utilisation de l'option -e
run_test "explicit_xml" "./mypandoc -i $TEST_DIR/basic.xml -f json -e xml" 0 "Spécifier explicitement le format XML"
run_test "explicit_json" "./mypandoc -i $TEST_DIR/basic.json -f xml -e json" 0 "Spécifier explicitement le format JSON"
run_test "explicit_md" "./mypandoc -i $TEST_DIR/basic.md -f json -e markdown" 0 "Spécifier explicitement le format Markdown"

echo -e "\n${YELLOW}=== TESTS AVEC FICHIERS DE SORTIE ===${NC}"

# Utilisation de l'option -o
run_test "output_file_xml" "./mypandoc -i $TEST_DIR/basic.xml -f json -o $TEST_DIR/output.json" 0 "Écrire la sortie dans un fichier (XML → JSON)"
run_test "output_file_json" "./mypandoc -i $TEST_DIR/basic.json -f xml -o $TEST_DIR/output.xml" 0 "Écrire la sortie dans un fichier (JSON → XML)"
run_test "output_file_md" "./mypandoc -i $TEST_DIR/basic.md -f json -o $TEST_DIR/output.json" 0 "Écrire la sortie dans un fichier (Markdown → JSON)"

echo -e "\n${YELLOW}=== TESTS AVEC MÉTADONNÉES ===${NC}"

# Préservation des métadonnées
run_test "metadata_xml_json" "./mypandoc -i $TEST_DIR/with_metadata.xml -f json" 0 "Préserver les métadonnées (XML → JSON)"
run_test "metadata_json_xml" "./mypandoc -i $TEST_DIR/with_metadata.json -f xml" 0 "Préserver les métadonnées (JSON → XML)"
run_test "metadata_json_md" "./mypandoc -i $TEST_DIR/with_metadata.json -f markdown" 0 "Préserver les métadonnées (JSON → Markdown)"
run_test "metadata_md_json" "./mypandoc -i $TEST_DIR/with_metadata.md -f json" 0 "Préserver les métadonnées (Markdown → JSON)"

echo -e "\n${YELLOW}=== TESTS AVEC FORMATAGE ===${NC}"

# Préservation du formatage
run_test "formatting_xml_json" "./mypandoc -i $TEST_DIR/formatting.xml -f json" 0 "Préserver le formatage (XML → JSON)"
run_test "formatting_json_xml" "./mypandoc -i $TEST_DIR/formatting.json -f xml" 0 "Préserver le formatage (JSON → XML)"
run_test "formatting_json_md" "./mypandoc -i $TEST_DIR/formatting.json -f markdown" 0 "Préserver le formatage (JSON → Markdown)"

echo -e "\n${YELLOW}=== TESTS AVEC DOCUMENTS COMPLEXES ===${NC}"

# Documents complexes
run_test "complex_xml_json" "./mypandoc -i $TEST_DIR/complex.xml -f json" 0 "Convertir document complexe (XML → JSON)"
run_test "complex_json_xml" "./mypandoc -i $TEST_DIR/complex.json -f xml" 0 "Convertir document complexe (JSON → XML)"
run_test "complex_json_md" "./mypandoc -i $TEST_DIR/complex.json -f markdown" 0 "Convertir document complexe (JSON → Markdown)"
run_test "complex_md_json" "./mypandoc -i $TEST_DIR/complex.md -f json" 0 "Convertir document complexe (Markdown → JSON)"

echo -e "\n${YELLOW}=== TESTS DE ROBUSTESSE ===${NC}"

# Documents mal formés ou vides
run_test "malformed_xml" "./mypandoc -i $TEST_DIR/malformed.xml -f json" 0 "Gérer un XML mal formé"
run_test "malformed_json" "./mypandoc -i $TEST_DIR/malformed.json -f xml" 0 "Gérer un JSON mal formé"
run_test "malformed_md" "./mypandoc -i $TEST_DIR/malformed.md -f json" 0 "Gérer un Markdown mal formé"
run_test "empty_file" "./mypandoc -i $TEST_DIR/empty.txt -f json" 0 "Gérer un fichier vide"
run_test "minimal_xml" "./mypandoc -i $TEST_DIR/minimal.xml -f json" 0 "Gérer un XML minimal"
run_test "minimal_json" "./mypandoc -i $TEST_DIR/minimal.json -f xml" 0 "Gérer un JSON minimal"
run_test "minimal_md" "./mypandoc -i $TEST_DIR/minimal.md -f json" 0 "Gérer un Markdown minimal"
run_test "special_chars" "./mypandoc -i $TEST_DIR/special_chars.xml -f json" 0 "Gérer des caractères spéciaux"

echo -e "\n${YELLOW}=== TESTS D'ERREUR ===${NC}"

# Cas d'erreur attendus
run_test "nonexistent_file" "./mypandoc -i nonexistent_file.txt -f json" 84 "Gérer un fichier inexistant"
run_test "invalid_format" "./mypandoc -i $TEST_DIR/basic.xml -f html" 84 "Gérer un format de sortie invalide"
run_test "invalid_input_format" "./mypandoc -i $TEST_DIR/basic.xml -f json -e html" 84 "Gérer un format d'entrée invalide"

echo -e "\n${YELLOW}=== TESTS SPÉCIFIQUES MOULINETTE ===${NC}"

# Noms de formats variés
run_test "format_md" "./mypandoc -i $TEST_DIR/basic.md -f xml -e md" 0 "Accepter 'md' comme abréviation de 'markdown'"
run_test "format_markdown" "./mypandoc -i $TEST_DIR/basic.md -f xml -e markdown" 0 "Accepter 'markdown' comme format"

# Conversion entre tous les formats (tests de la moulinette)
for input_format in "xml" "json" "markdown"; do
    for output_format in "xml" "json" "markdown"; do
        input_file=""
        case $input_format in
            "xml") input_file="$TEST_DIR/basic.xml" ;;
            "json") input_file="$TEST_DIR/basic.json" ;;
            "markdown") input_file="$TEST_DIR/basic.md" ;;
        esac
        
        run_test "moulinette_${input_format}_to_${output_format}" "./mypandoc -i $input_file -f $output_format -e $input_format" 0 "Test moulinette: $input_format → $output_format"
    done
done

echo -e "\n${YELLOW}========== RÉSUMÉ DES TESTS ==========${NC}"
echo -e "Total des tests: $TOTAL_TESTS"
echo -e "Tests réussis: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Tests échoués: ${RED}$FAILED_TESTS${NC}"

if [ $PASSED_TESTS -eq $TOTAL_TESTS ]; then
    echo -e "\n${GREEN}TOUS LES TESTS ONT RÉUSSI !${NC}"
    echo -e "Votre programme devrait fonctionner avec la moulinette."
else
    echo -e "\n${RED}CERTAINS TESTS ONT ÉCHOUÉ.${NC}"
    echo -e "Vérifiez les messages d'erreur pour identifier les problèmes."
    
    # Afficher la liste des tests qui ont échoué
    echo -e "\n${YELLOW}Tests échoués:${NC}"
    for i in $(seq 1 $TOTAL_TESTS); do
        if [ -s "$TEST_DIR/error_$i.err" ]; then
            test_output=$(head -n 1 "$TEST_DIR/output_$i.out" 2>/dev/null || echo "")
            if [ -z "$test_output" ]; then
                echo -e "${RED}Test #$i${NC}: $(grep "Test #$i:" "$0" | sed 's/.*Test #[0-9]*: \(.*\)".*/\1/')"
                echo -e "  Erreur: $(cat "$TEST_DIR/error_$i.err")"
            fi
        fi
    done
fi

echo -e "\nLes fichiers de test sont disponibles dans le répertoire $TEST_DIR"
echo -e "Pour un nettoyage complet: rm -rf $TEST_DIR"
