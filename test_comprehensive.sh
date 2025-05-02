#!/bin/bash
# Test script complet pour myPandoc couvrant des cas simples à complexes

# Couleurs pour une meilleure lisibilité
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Compteurs pour le suivi des tests
TOTAL_TESTS=0
PASSED_TESTS=0

# Créer le répertoire de test
TEST_DIR="comprehensive_tests"
mkdir -p $TEST_DIR

echo -e "${YELLOW}===== CRÉATION DES FICHIERS DE TEST =====${NC}"

# 1. CAS SIMPLE: Document minimal
echo -e "\n${BLUE}Création des fichiers de test simples...${NC}"

# XML minimal
cat > $TEST_DIR/minimal.xml << 'EOF'
<document>
<header title="Minimal"></header>
<body>
<paragraph>Test minimal.</paragraph>
</body>
</document>
EOF

# JSON minimal
cat > $TEST_DIR/minimal.json << 'EOF'
{
  "header": {
    "title": "Minimal"
  },
  "body": [
    "Test minimal."
  ]
}
EOF

# Markdown minimal
cat > $TEST_DIR/minimal.md << 'EOF'
---
title: Minimal
---

Test minimal.
EOF

# 2. CAS INTERMÉDIAIRE: Document avec métadonnées et formatage
echo -e "\n${BLUE}Création des fichiers de test intermédiaires...${NC}"

# XML intermédiaire
cat > $TEST_DIR/intermediate.xml << 'EOF'
<document>
<header title="Document Intermédiaire" author="Testeur" date="2025-05-02"></header>
<body>
<paragraph>Ce document contient du <bold>texte en gras</bold> et du <italic>texte en italique</italic>.</paragraph>
<paragraph>Il possède aussi du <code>code inline</code> à tester.</paragraph>
</body>
</document>
EOF

# JSON intermédiaire
cat > $TEST_DIR/intermediate.json << 'EOF'
{
  "header": {
    "title": "Document Intermédiaire",
    "author": "Testeur",
    "date": "2025-05-02"
  },
  "body": [
    {
      "type": "paragraph",
      "content": [
        "Ce document contient du ",
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
    },
    {
      "type": "paragraph",
      "content": [
        "Il possède aussi du ",
        {
          "type": "code",
          "content": "code inline"
        },
        " à tester."
      ]
    }
  ]
}
EOF

# Markdown intermédiaire
cat > $TEST_DIR/intermediate.md << 'EOF'
---
title: Document Intermédiaire
author: Testeur
date: 2025-05-02
---

Ce document contient du **texte en gras** et du *texte en italique*.

Il possède aussi du `code inline` à tester.
EOF

# 3. CAS COMPLEXE: Document avec sections, listes, liens et images
echo -e "\n${BLUE}Création des fichiers de test complexes...${NC}"

# XML complexe
cat > $TEST_DIR/complex.xml << 'EOF'
<document>
<header title="Document Complexe" author="Testeur Avancé" date="2025-05-02"></header>
<body>
<section title="Introduction">
  <paragraph>Ce document contient divers éléments structurés.</paragraph>
</section>
<section title="Listes">
  <paragraph>Voici une liste d'éléments :</paragraph>
  <list>
    <item>Premier élément de la liste</item>
    <item>Deuxième élément avec du <bold>formatage</bold></item>
    <item>Troisième élément avec un <link url="https://example.com">lien</link></item>
  </list>
</section>
<section title="Code">
  <paragraph>Exemple de bloc de code :</paragraph>
  <codeblock>
function test() {
  console.log("Hello world");
  return true;
}
  </codeblock>
</section>
<section title="Médias">
  <paragraph>Une image : <image url="https://example.com/image.png">Description de l'image</image></paragraph>
</section>
</body>
</document>
EOF

# JSON complexe
cat > $TEST_DIR/complex.json << 'EOF'
{
  "header": {
    "title": "Document Complexe",
    "author": "Testeur Avancé",
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
            "Ce document contient divers éléments structurés."
          ]
        }
      ]
    },
    {
      "type": "section",
      "title": "Listes",
      "content": [
        {
          "type": "paragraph",
          "content": [
            "Voici une liste d'éléments :"
          ]
        },
        {
          "type": "list",
          "items": [
            {
              "content": [
                "Premier élément de la liste"
              ]
            },
            {
              "content": [
                "Deuxième élément avec du ",
                {
                  "type": "bold",
                  "content": "formatage"
                }
              ]
            },
            {
              "content": [
                "Troisième élément avec un ",
                {
                  "type": "link",
                  "text": "lien",
                  "url": "https://example.com"
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "type": "section",
      "title": "Code",
      "content": [
        {
          "type": "paragraph",
          "content": [
            "Exemple de bloc de code :"
          ]
        },
        {
          "type": "codeblock",
          "content": "function test() {\n  console.log(\"Hello world\");\n  return true;\n}"
        }
      ]
    },
    {
      "type": "section",
      "title": "Médias",
      "content": [
        {
          "type": "paragraph",
          "content": [
            "Une image : ",
            {
              "type": "image",
              "alt": "Description de l'image",
              "url": "https://example.com/image.png"
            }
          ]
        }
      ]
    }
  ]
}
EOF

# Markdown complexe
cat > $TEST_DIR/complex.md << 'EOF'
---
title: Document Complexe
author: Testeur Avancé
date: 2025-05-02
---

# Introduction

Ce document contient divers éléments structurés.

# Listes

Voici une liste d'éléments :

- Premier élément de la liste
- Deuxième élément avec du **formatage**
- Troisième élément avec un [lien](https://example.com)

# Code

Exemple de bloc de code :

```
function test() {
  console.log("Hello world");
  return true;
}
```

# Médias

Une image : ![Description de l'image](https://example.com/image.png)
EOF

# 4. CAS EDGE: Documents spéciaux pour tester les cas limites
echo -e "\n${BLUE}Création des fichiers pour les cas limites...${NC}"

# XML vide
cat > $TEST_DIR/empty.xml << 'EOF'
<document>
<header title=""></header>
<body>
</body>
</document>
EOF

# JSON avec structure minimale
cat > $TEST_DIR/minimal_structure.json << 'EOF'
{"header":{"title":"Minimal"},"body":[]}
EOF

# Markdown avec espaces en trop
cat > $TEST_DIR/whitespace.md << 'EOF'

   
---
title:    Espaces   
   author:    Test   
---
   

   Texte avec des    espaces    supplémentaires.   

EOF

# Document avec caractères spéciaux
cat > $TEST_DIR/special_chars.xml << 'EOF'
<document>
<header title="Caractères &amp; Spéciaux"></header>
<body>
<paragraph>Test avec &lt;caractères&gt; spéciaux &amp; symboles : ü, é, è, à, ñ, ç, €, £, ¥.</paragraph>
</body>
</document>
EOF

# Fonction pour exécuter un test
run_test() {
    local test_name=$1
    local input_file=$2
    local output_format=$3
    local input_format=$4
    local options=$5
    
    ((TOTAL_TESTS++))
    
    local output_file="$TEST_DIR/output_${test_name}.${output_format}"
    local input_format_option=""
    if [ -n "$input_format" ]; then
        input_format_option="-e $input_format"
    fi
    
    echo -e "\n${BLUE}[Test $TOTAL_TESTS] $test_name${NC}"
    echo -e "File: $input_file → $output_format"
    
    local cmd="./mypandoc -i $input_file -f $output_format -o $output_file $input_format_option $options"
    echo -e "Command: $cmd"
    
    # Exécution avec capture de la sortie et du code de retour
    eval $cmd > "$TEST_DIR/stdout_${test_name}.log" 2> "$TEST_DIR/stderr_${test_name}.log"
    local exit_code=$?
    
    if [ $exit_code -eq 0 ] && [ -s "$output_file" ]; then
        echo -e "${GREEN}✓ SUCCÈS${NC} (code: $exit_code)"
        ((PASSED_TESTS++))
    else
        echo -e "${RED}✗ ÉCHEC${NC} (code: $exit_code)"
        echo -e "Erreur:"
        cat "$TEST_DIR/stderr_${test_name}.log"
    fi
    
    # Résumer le contenu de sortie (premières lignes)
    if [ -s "$output_file" ]; then
        echo -e "${BLUE}Contenu (aperçu):${NC}"
        head -n 5 "$output_file"
        if [ $(wc -l < "$output_file") -gt 5 ]; then
            echo -e "${BLUE}...${NC}"
        fi
        echo ""
    fi
}

echo -e "\n${YELLOW}===== EXÉCUTION DES TESTS =====${NC}"

# Tests par niveau de complexité
echo -e "\n${YELLOW}--- TESTS DE NIVEAU 1: DOCUMENTS MINIMAUX ---${NC}"
run_test "minimal_xml_to_json" "$TEST_DIR/minimal.xml" "json" "xml" ""
run_test "minimal_xml_to_md" "$TEST_DIR/minimal.xml" "markdown" "xml" ""
run_test "minimal_json_to_xml" "$TEST_DIR/minimal.json" "xml" "json" ""
run_test "minimal_json_to_md" "$TEST_DIR/minimal.json" "markdown" "json" ""
run_test "minimal_md_to_xml" "$TEST_DIR/minimal.md" "xml" "markdown" ""
run_test "minimal_md_to_json" "$TEST_DIR/minimal.md" "json" "markdown" ""

echo -e "\n${YELLOW}--- TESTS DE NIVEAU 2: DOCUMENTS INTERMÉDIAIRES ---${NC}"
run_test "inter_xml_to_json" "$TEST_DIR/intermediate.xml" "json" "xml" ""
run_test "inter_xml_to_md" "$TEST_DIR/intermediate.xml" "markdown" "xml" ""
run_test "inter_json_to_xml" "$TEST_DIR/intermediate.json" "xml" "json" ""
run_test "inter_json_to_md" "$TEST_DIR/intermediate.json" "markdown" "json" ""
run_test "inter_md_to_xml" "$TEST_DIR/intermediate.md" "xml" "markdown" ""
run_test "inter_md_to_json" "$TEST_DIR/intermediate.md" "json" "markdown" ""

echo -e "\n${YELLOW}--- TESTS DE NIVEAU 3: DOCUMENTS COMPLEXES ---${NC}"
run_test "complex_xml_to_json" "$TEST_DIR/complex.xml" "json" "xml" ""
run_test "complex_xml_to_md" "$TEST_DIR/complex.xml" "markdown" "xml" ""
run_test "complex_json_to_xml" "$TEST_DIR/complex.json" "xml" "json" ""
run_test "complex_json_to_md" "$TEST_DIR/complex.json" "markdown" "json" ""
run_test "complex_md_to_xml" "$TEST_DIR/complex.md" "xml" "markdown" ""
run_test "complex_md_to_json" "$TEST_DIR/complex.md" "json" "markdown" ""

echo -e "\n${YELLOW}--- TESTS DE NIVEAU 4: CAS LIMITES ---${NC}"
run_test "empty_xml_to_json" "$TEST_DIR/empty.xml" "json" "xml" ""
run_test "min_struct_json_to_xml" "$TEST_DIR/minimal_structure.json" "xml" "json" ""
run_test "whitespace_md_to_json" "$TEST_DIR/whitespace.md" "json" "markdown" ""
run_test "special_chars_xml_to_md" "$TEST_DIR/special_chars.xml" "markdown" "xml" ""

echo -e "\n${YELLOW}--- TESTS DE DÉTECTION AUTOMATIQUE DE FORMAT ---${NC}"
run_test "auto_detect_xml" "$TEST_DIR/complex.xml" "json" "" ""
run_test "auto_detect_json" "$TEST_DIR/complex.json" "xml" "" ""
run_test "auto_detect_md" "$TEST_DIR/complex.md" "json" "" ""

echo -e "\n${YELLOW}--- TESTS D'ERREUR ---${NC}"
run_test "nonexistent_file" "nonexistent_file.xml" "json" "" ""
run_test "invalid_format" "$TEST_DIR/minimal.xml" "html" "" ""

echo -e "\n${YELLOW}===== RÉSUMÉ DES TESTS =====${NC}"
echo -e "Total des tests: $TOTAL_TESTS"
echo -e "Tests réussis: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Tests échoués: ${RED}$((TOTAL_TESTS - PASSED_TESTS))${NC}"

if [ $PASSED_TESTS -eq $TOTAL_TESTS ]; then
    echo -e "\n${GREEN}TOUS LES TESTS ONT RÉUSSI!${NC}"
else
    echo -e "\n${RED}CERTAINS TESTS ONT ÉCHOUÉ.${NC} Vérifiez les détails ci-dessus."
    echo -e "Les fichiers de sortie, stdout et stderr sont disponibles dans le répertoire $TEST_DIR"
fi

echo -e "\n${YELLOW}===== TESTS ADDITIONNELS DE LA MOULINETTE =====${NC}"
echo -e "${BLUE}Ces tests spécifiques simulent les tests possibles de la moulinette${NC}"

# Teste la conversion entre tous les types avec des fichiers qui fonctionnent
run_test "moulinette_basic_xml" "$TEST_DIR/minimal.xml" "xml" "xml" ""
run_test "moulinette_basic_json" "$TEST_DIR/minimal.json" "json" "json" ""
run_test "moulinette_basic_md" "$TEST_DIR/minimal.md" "markdown" "markdown" ""

# Tests spécifiques à chaque type de document
run_test "moulinette_empty_body" "$TEST_DIR/empty.xml" "markdown" "" ""
run_test "moulinette_minimal_struct" "$TEST_DIR/minimal_structure.json" "markdown" "" ""

echo -e "\n${YELLOW}===== NETTOYAGE =====${NC}"
echo -e "Les fichiers de test ont été conservés dans le répertoire $TEST_DIR pour analyse."
echo -e "Pour les supprimer: rm -rf $TEST_DIR"

# Fin du script
echo -e "\n${GREEN}Fin des tests.${NC}"