module Utils = Extension_utils
module Runtime = Runtime_chrome_lwt
module Tabs = Tabs_chrome_lwt
module Storage = Storage_chrome_lwt
module Windows = Windows_chrome_lwt
module I18n = I18n_chrome_lwt

let runtime = Runtime.runtime
let storage = Storage.storage
let sync = Storage.sync
let local = Storage.local
let tabs = Tabs.tabs
let windows = Windows.windows
let i18n = I18n.i18n
