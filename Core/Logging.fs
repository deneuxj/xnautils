module CleverRake.XnaUtils.Logging

open Units

type Queue<'M> = CircularQueue.CircularQueue<'M>

type LogContent<'M> =
    { capacity : int
      messageLifeTime : float32<s>
      mutable messages : Queue<'M>
      mutable timeLeft : Queue<float32<s>> }

let numMessagesIn content =
    content.messages.len

let messagesIn content =
    CircularQueue.content content.messages

let addMessageTo content msg =
    CircularQueue.add content.messages msg
    CircularQueue.add content.timeLeft content.messageLifeTime

let update dt content =
    for i in CircularQueue.validIndices content.timeLeft do
        content.timeLeft.content.[i] <- content.timeLeft.content.[i] - dt

    // Remove stale messages
    let rec checkEmpty() =
        if CircularQueue.isEmpty content.timeLeft then
            ()
        else
            filter()

    and filter() =
        let first = content.timeLeft.content.[content.timeLeft.first]
        if first <= 0.0f<s> then
            CircularQueue.pick content.messages |> ignore
            CircularQueue.pick content.timeLeft |> ignore
            checkEmpty()

    checkEmpty()