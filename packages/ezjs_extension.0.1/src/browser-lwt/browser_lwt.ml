module Utils = Extension_utils
module Runtime = Runtime_browser_lwt
module Tabs = Tabs_browser_lwt
module Storage = Storage_browser_lwt
module Windows = Windows_browser_lwt
module I18n = I18n_browser_lwt

let storage  = Storage.storage
let sync = Storage.sync
let local = Storage.local
let tabs = Tabs.tabs
let runtime = Runtime.runtime
let windows = Windows.windows
let i18n = I18n.i18n
