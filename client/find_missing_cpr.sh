#!/bin/bash

STMTTS='/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
'
STMTVUE='<!--
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
-->
'

for f in $(find ./src -type f -name "*.ts"); do
  if ! grep "Copyright 2020 Florian Kramer" "$f" > /dev/null; then 
    OLD=$(cat "$f")
    echo "$STMTTS" > "$f"
    echo "$OLD" >> "$f"
    echo "$f is missing the copyright statement" 
  fi 
done

for f in $(find ./src -type f -name "*.vue"); do
  if ! grep "Copyright 2020 Florian Kramer" "$f" > /dev/null; then 
    OLD=$(cat "$f")
    echo "$STMTVUE" > "$f"
    echo "$OLD" >> "$f"
    echo "$f is missing the copyright statement" 
  fi 
done
