#!/usr/bin/env node
// test-config.js - Test configuration loading

const vscode = require('vscode')

// Mock VS Code API for testing
const mockVscode = {
  workspace: {
    getConfiguration: (section) => {
      const config = {
        get: (key, defaultValue) => {
          console.log(`Getting config: ${section}.${key}, default: ${defaultValue}`)

          // Mock some test values
          if (section === 'coscad' && key === 'executablePath') {
            return '/custom/path/to/coscad'
          }
          if (section === 'coscad' && key === 'executableCommand') {
            return 'custom-coscad-command'
          }

          return defaultValue
        }
      }
      return config
    }
  }
}

// Test configuration loading
console.log('Testing COSCAD configuration loading...')
const config = mockVscode.workspace.getConfiguration('coscad')
const executablePath = config.get('executablePath', '/default/path')
const executableCommand = config.get('executableCommand', 'default-command')

console.log(`Executable Path: ${executablePath}`)
console.log(`Executable Command: ${executableCommand}`)

console.log('\nConfiguration test completed successfully!')
