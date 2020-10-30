module Utils = Extension_utils
module Runtime = Runtime_browser
module Tabs = Tabs_browser
module Storage = Storage_browser
module Windows = Windows_browser
module I18n = I18n_browser
module Browser_action = Browser_action_browser

let storage  = Storage.storage
let sync = Storage.sync
let local = Storage.local
let tabs = Tabs.tabs
let runtime = Runtime.runtime
let windows = Windows.windows
let i18n = I18n.i18n
