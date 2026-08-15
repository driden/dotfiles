import { spawn } from "node:child_process"
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent"

export type NotificationPayload = {
  app: string
  cwd: string
  event: string
  message: string
}

const NOTIFIER = `${process.env.HOME}/.local/bin/notify-tmux.sh`

export function createPiPayload(cwd: string): NotificationPayload {
  return {
    app: "Pi",
    cwd,
    event: "Finished",
    message: "Task finished",
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

export default function notifyTmux(pi: ExtensionAPI) {
  pi.on("agent_settled", async (_event, ctx) => {
    await notify(createPiPayload(ctx.cwd))
  })
}
