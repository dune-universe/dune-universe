module Utils = Extension_utils
module Runtime = Runtime_chrome
module Tabs = Tabs_chrome
module Storage = Storage_chrome
module Windows = Windows_chrome
module I18n = I18n_chrome
module Browser_action = Browser_action_chrome

let runtime = Runtime.runtime
let storage = Storage.storage
let sync = Storage.sync
let local = Storage.local
let tabs = Tabs.tabs
let windows = Windows.windows
let i18n = I18n.i18n
