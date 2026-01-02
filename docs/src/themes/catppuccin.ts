/**
 * Catppuccin color palettes for Monaco Editor
 * https://github.com/catppuccin/catppuccin
 *
 * MIT License
 *
 * Copyright (c) 2021 Catppuccin
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

import type * as monaco from "monaco-editor";

export const macchiato = {
  rosewater: "#f4dbd6",
  flamingo: "#f0c6c6",
  pink: "#f5bde6",
  mauve: "#c6a0f6",
  red: "#ed8796",
  maroon: "#ee99a0",
  peach: "#f5a97f",
  yellow: "#eed49f",
  green: "#a6da95",
  teal: "#8bd5ca",
  sky: "#91d7e3",
  sapphire: "#7dc4e4",
  blue: "#8aadf4",
  lavender: "#b7bdf8",
  text: "#cad3f5",
  subtext1: "#b8c0e0",
  subtext0: "#a5adcb",
  overlay2: "#939ab7",
  overlay1: "#8087a2",
  overlay0: "#6e738d",
  surface2: "#5b6078",
  surface1: "#494d64",
  surface0: "#363a4f",
  base: "#24273a",
  mantle: "#1e2030",
  crust: "#181926",
} as const;

export const latte = {
  rosewater: "#dc8a78",
  flamingo: "#dd7878",
  pink: "#ea76cb",
  mauve: "#8839ef",
  red: "#d20f39",
  maroon: "#e64553",
  peach: "#fe640b",
  yellow: "#df8e1d",
  green: "#40a02b",
  teal: "#179299",
  sky: "#04a5e5",
  sapphire: "#209fb5",
  blue: "#1e66f5",
  lavender: "#7287fd",
  text: "#4c4f69",
  subtext1: "#5c5f77",
  subtext0: "#6c6f85",
  overlay2: "#7c7f93",
  overlay1: "#8c8fa1",
  overlay0: "#9ca0b0",
  surface2: "#acb0be",
  surface1: "#bcc0cc",
  surface0: "#ccd0da",
  base: "#eff1f5",
  mantle: "#e6e9ef",
  crust: "#dce0e8",
} as const;

// Widen literal types to string while preserving keys
type Palette = { [K in keyof typeof macchiato]: string };

function createTheme(palette: Palette, base: "vs" | "vs-dark"): monaco.editor.IStandaloneThemeData {
  return {
    base,
    inherit: true,
    rules: [
      { token: "keyword", foreground: palette.mauve.slice(1) },
      { token: "type.identifier", foreground: palette.yellow.slice(1) },
      { token: "identifier", foreground: palette.text.slice(1) },
      { token: "string", foreground: palette.green.slice(1) },
      { token: "string.escape", foreground: palette.pink.slice(1) },
      { token: "number", foreground: palette.peach.slice(1) },
      { token: "number.float", foreground: palette.peach.slice(1) },
      { token: "number.hex", foreground: palette.peach.slice(1) },
      { token: "comment", foreground: palette.overlay1.slice(1) },
      { token: "operator", foreground: palette.sky.slice(1) },
    ],
    colors: {
      "editor.background": palette.base,
      "editor.foreground": palette.text,
      "editor.lineHighlightBackground": palette.text + "12",
      "editor.selectionBackground": palette.overlay2 + (base === "vs" ? "4D" : "40"),
      "editor.wordHighlightBackground": palette.overlay2 + "33",
      "editor.wordHighlightStrongBackground": palette.blue + (base === "vs" ? "26" : "33"),
      "editorCursor.foreground": palette.rosewater,
      "editorLineNumber.foreground": palette.overlay0,
      "editorLineNumber.activeForeground": palette.lavender,
    },
  };
}

export const catppuccinMacchiato = createTheme(macchiato, "vs-dark");
export const catppuccinLatte = createTheme(latte, "vs");
