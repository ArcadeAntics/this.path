# based on a 1920 x 1080 monitor


{
    rMouse::setAutoDelay(100)
    this.path:::Rgui("--vanilla", wait = FALSE, quiet = TRUE)


    # 'File' button
    # rMouse::move(  0,  23)
    # rMouse::move( 31,  23)
    # rMouse::move(  0,  41)
    # rMouse::move( 31,  41)
    rMouse::move( 15,  32)
    rMouse::left()


    # 'New script' button
    # rMouse::move(  3,  67)
    # rMouse::move(216,  67)
    # rMouse::move(  3,  88)
    # rMouse::move(216,  88)
    rMouse::move(109,  77)
    rMouse::left()


    # Select R Console
    # rMouse::move(  0,  78)
    # rMouse::move(680,  78)
    # rMouse::move(  0, 549)
    # rMouse::move(680, 549)
    rMouse::move(340, 313)
    rMouse::left()
}
