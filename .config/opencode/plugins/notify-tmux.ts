import { spawn } from "node:child_process"

export type NotificationPayload = {
  app: string
  cwd: string
  event: string
  message: string
}

type CurrentEvent = {
  type?: string
  properties?: {
    status?: { type?: string }
    action?: string
    permission?: string
    questions?: Array<{ header?: string; question?: string }>
  }
}

type NextEvent = {
  type?: string
  location?: { directory?: string }
  data?: {
    sessionID?: string
    action?: string
    questions?: Array<{ header?: string; question?: string }>
    form?: { title?: string }
  }
}

const NOTIFIER = `${process.env.HOME}/.local/bin/notify-tmux.sh`

function inputMessage(action: string | undefined, fallback: string): string {
  return action ? `${action} needs your approval` : fallback
}

export function normalizeOpenCodeEvent(
  event: CurrentEvent,
  cwd: string,
): NotificationPayload | undefined {
  if (event.type === "session.status" && event.properties?.status?.type === "idle") {
    return { app: "OpenCode", cwd, event: "Finished", message: "Task finished" }
  }

  if (event.type === "permission.v2.asked" || event.type === "permission.asked") {
    const action = event.properties?.action ?? event.properties?.permission
    return {
      app: "OpenCode",
      cwd,
      event: "Permission",
      message: inputMessage(action, "Needs your approval"),
    }
  }

  if (event.type === "question.v2.asked" || event.type === "question.asked") {
    const question = event.properties?.questions?.[0]
    return {
      app: "OpenCode",
      cwd,
      event: "Question",
      message: question?.header ?? question?.question ?? "Needs your input",
    }
  }
}

export function normalizeOpenCode2Event(event: NextEvent): NotificationPayload | undefined {
  const cwd = event.location?.directory ?? process.cwd()

  if (event.type === "session.execution.succeeded") {
    return { app: "OpenCode2", cwd, event: "Finished", message: "Task finished" }
  }

  if (event.type === "permission.asked") {
    return {
      app: "OpenCode2",
      cwd,
      event: "Permission",
      message: inputMessage(event.data?.action, "Needs your approval"),
    }
  }

  if (event.type === "form.created") {
    return {
      app: "OpenCode2",
      cwd,
      event: "Question",
      message: event.data?.form?.title ?? "Needs your input",
    }
  }

  if (event.type === "question.asked") {
    const question = event.data?.questions?.[0]
    return {
      app: "OpenCode2",
      cwd,
      event: "Question",
      message: question?.header ?? question?.question ?? "Needs your input",
    }
  }
}

async function notify(payload: NotificationPayload): Promise<void> {
  await new Promise<void>((resolve) => {
    try {
      const child = spawn(NOTIFIER, [], {
        env: process.env,
        stdio: ["pipe", "ignore", "ignore"],
      })
      child.once("error", resolve)
      child.once("close", resolve)
      child.stdin.once("error", resolve)
      child.stdin.end(JSON.stringify(payload))
    } catch {
      resolve()
    }
  })
}

async function server(input: { directory: string }) {
  return {
    event: async ({ event }: { event: CurrentEvent }) => {
      const payload = normalizeOpenCodeEvent(event, input.directory)
      if (payload) await notify(payload)
    },
  }
}

async function setup(context: {
  event: {
    subscribe(options?: {
      signal?: AbortSignal
    }): Promise<AsyncIterable<NextEvent>> | AsyncIterable<NextEvent>
  }
}) {
  const controller = new AbortController()
  const subscription = await context.event.subscribe({ signal: controller.signal })
  const stream =
    (subscription as AsyncIterable<NextEvent> & { stream?: AsyncIterable<NextEvent> }).stream ??
    subscription
  const iterator = stream[Symbol.asyncIterator]()
  let stopped = false

  void (async () => {
    try {
      while (!stopped) {
        const next = await iterator.next()
        if (next.done) break
        const payload = normalizeOpenCode2Event(next.value)
        if (payload) await notify(payload)
      }
    } catch {
      // Notifications must never interrupt OpenCode.
    }
  })()

  return () => {
    stopped = true
    controller.abort()
  }
}

export default {
  id: "notify-tmux",
  server,
  setup,
}
