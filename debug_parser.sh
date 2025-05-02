#!/bin/bash

# Colors for better output readability
GREEN='\033[0;32m'
RED='\033[0;31m'
BLUE='\033[0;34m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

# Test directory
TEST_DIR="debug_files"
mkdir -p $TEST_DIR

echo -e "${YELLOW}===== XML Parser Debugging =====${NC}"

# Create increasingly complex XML files to identify where parsing fails
echo -e "Creating test XML files with increasing complexity..."

# Test 1: Minimal XML document
cat > $TEST_DIR/xml_1_minimal.xml << 'EOF'
<document>
<header title="Minimal Example"></header>
<body>
<paragraph>Test</paragraph>
</body>
</document>
EOF

# Test 2: XML with author and date
cat > $TEST_DIR/xml_2_with_meta.xml << 'EOF'
<document>
<header title="With Metadata">
<author>Test Author</author>
<date>2025-05-02</date>
</header>
<body>
<paragraph>Test</paragraph>
</body>
</document>
EOF

# Test 3: XML with basic formatting
cat > $TEST_DIR/xml_3_formatting.xml << 'EOF'
<document>
<header title="With Formatting"></header>
<body>
<paragraph>This is <bold>bold</bold> and <italic>italic</italic> text.</paragraph>
</body>
</document>
EOF

# Test 4: XML with sections
cat > $TEST_DIR/xml_4_sections.xml << 'EOF'
<document>
<header title="With Sections"></header>
<body>
<section title="Section 1">
<paragraph>Section content</paragraph>
</section>
</body>
</document>
EOF

# Test 5: XML with list
cat > $TEST_DIR/xml_5_list.xml << 'EOF'
<document>
<header title="With List"></header>
<body>
<list>
<paragraph>Item 1</paragraph>
<paragraph>Item 2</paragraph>
</list>
</body>
</document>
EOF

# Test 6: XML with link and image
cat > $TEST_DIR/xml_6_link_image.xml << 'EOF'
<document>
<header title="With Link and Image"></header>
<body>
<paragraph>This has a <link url="https://example.com">link</link>.</paragraph>
<paragraph>This has an <image url="https://example.com/img.png">alt text</image>.</paragraph>
</body>
</document>
EOF

echo -e "${YELLOW}===== JSON Parser Debugging =====${NC}"

# Create increasingly complex JSON files to identify where parsing fails
echo -e "Creating test JSON files with increasing complexity..."

# Test 1: Minimal JSON document
cat > $TEST_DIR/json_1_minimal.json << 'EOF'
{
  "header": {
    "title": "Minimal Example"
  },
  "body": [
    "Test"
  ]
}
EOF

# Test 2: JSON with author and date
cat > $TEST_DIR/json_2_with_meta.json << 'EOF'
{
  "header": {
    "title": "With Metadata",
    "author": "Test Author",
    "date": "2025-05-02"
  },
  "body": [
    "Test"
  ]
}
EOF

# Test 3: JSON with basic formatting
cat > $TEST_DIR/json_3_formatting.json << 'EOF'
{
  "header": {
    "title": "With Formatting"
  },
  "body": [
    [
      "This is ",
      {
        "bold": "bold"
      },
      " and ",
      {
        "italic": "italic"
      },
      " text."
    ]
  ]
}
EOF

# Test 4: JSON with sections
cat > $TEST_DIR/json_4_sections.json << 'EOF'
{
  "header": {
    "title": "With Sections"
  },
  "body": [
    {
      "section": {
        "title": "Section 1",
        "content": [
          "Section content"
        ]
      }
    }
  ]
}
EOF

# Test 5: JSON with list
cat > $TEST_DIR/json_5_list.json << 'EOF'
{
  "header": {
    "title": "With List"
  },
  "body": [
    {
      "list": [
        [
          "Item 1"
        ],
        [
          "Item 2"
        ]
      ]
    }
  ]
}
EOF

# Test 6: JSON with link and image
cat > $TEST_DIR/json_6_link_image.json << 'EOF'
{
  "header": {
    "title": "With Link and Image"
  },
  "body": [
    [
      "This has a ",
      {
        "link": {
          "url": "https://example.com",
          "content": [
            "link"
          ]
        }
      },
      "."
    ],
    [
      "This has an ",
      {
        "image": {
          "url": "https://example.com/img.png",
          "alt": [
            "alt text"
          ]
        }
      },
      "."
    ]
  ]
}
EOF

# Run all XML tests
echo -e "\n${YELLOW}===== Running XML Parser Tests =====${NC}"
for i in {1..6}; do
  file="$TEST_DIR/xml_${i}_*.xml"
  file=$(echo $file) # Expand glob pattern
  
  echo -e "\n${BLUE}Testing: $file${NC}"
  echo -e "Command: ./mypandoc -i $file -f markdown -e xml"
  
  ./mypandoc -i $file -f markdown -e xml
  result=$?
  
  if [ $result -eq 0 ]; then
    echo -e "${GREEN}SUCCESS: Test passed${NC}"
  else
    echo -e "${RED}FAILED: Exit code $result${NC}"
  fi
done

# Run all JSON tests
echo -e "\n${YELLOW}===== Running JSON Parser Tests =====\n${NC}"
for i in {1..6}; do
  file="$TEST_DIR/json_${i}_*.json"
  file=$(echo $file) # Expand glob pattern
  
  echo -e "\n${BLUE}Testing: $file${NC}"
  echo -e "Command: ./mypandoc -i $file -f markdown -e json"
  
  ./mypandoc -i $file -f markdown -e json
  result=$?
  
  if [ $result -eq 0 ]; then
    echo -e "${GREEN}SUCCESS: Test passed${NC}"
  else
    echo -e "${RED}FAILED: Exit code $result${NC}"
  fi
done

# Test with verbose output if available
echo -e "\n${YELLOW}===== Testing with Simplified Files and Verbose Output =====\n${NC}"

# Create extremely minimal test files
cat > $TEST_DIR/minimal.xml << 'EOF'
<document><header title="Test"></header><body><paragraph>Test</paragraph></body></document>
EOF

cat > $TEST_DIR/minimal.json << 'EOF'
{"header":{"title":"Test"},"body":["Test"]}
EOF

echo -e "${BLUE}Testing minimal XML with verbose output:${NC}"
echo -e "Command: ./mypandoc -i $TEST_DIR/minimal.xml -f markdown -e xml -v"
./mypandoc -i $TEST_DIR/minimal.xml -f markdown -e xml -v 2>&1 || echo -e "${RED}Command failed${NC}"

echo -e "\n${BLUE}Testing minimal JSON with verbose output:${NC}"
echo -e "Command: ./mypandoc -i $TEST_DIR/minimal.json -f markdown -e json -v"
./mypandoc -i $TEST_DIR/minimal.json -f markdown -e json -v 2>&1 || echo -e "${RED}Command failed${NC}"

echo -e "\n${YELLOW}===== Debugging Complete =====\n${NC}"
echo "This script created test files with increasing complexity to help identify where the parsing fails."
echo "Check the output above to see which tests passed and which failed."
echo "Look at the test files in the $TEST_DIR directory to see their structure."
echo -e "${BLUE}Next steps:${NC}"
echo "1. Fix parsing issues in the simplest failing file first"
echo "2. Make sure your XML and JSON parsers handle all the required document elements"
echo "3. Once basic parsing works, try the full syntax examples again"