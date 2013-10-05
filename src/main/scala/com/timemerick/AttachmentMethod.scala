package com.timemerick

/**
 * User: temerick
 * Date: 10/5/13
 * Time: 1:15 PM
 */
sealed trait AttachmentMethod
object RightToLeft extends AttachmentMethod
object LeftToRight extends AttachmentMethod
object TopToBottom extends AttachmentMethod
object BottomToTop extends AttachmentMethod
