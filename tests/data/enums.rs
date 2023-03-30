#[minwin]
mod enums {
    enum WIN32_ERROR {
        ERROR_OUTOFMEMORY,
    }

    fn GetLastError() {}

    enum DUPLICATE_HANDLE_OPTIONS {}

    enum FILE_NOTIFY_CHANGE {
        FILE_NOTIFY_CHANGE_FILE_NAME,
        FILE_NOTIFY_CHANGE_LAST_WRITE,
        FILE_NOTIFY_CHANGE_SIZE,
    }
}
