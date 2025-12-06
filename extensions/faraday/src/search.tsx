import { ActionPanel, Action, Icon, List, Color } from "@raycast/api";
import { useEffect, useState } from "react";

const SCHEMA_URL = "https://app.faraday.ai/api.json";
const PAGE_URL = "https://faraday.ai/docs/reference/";

type OpenAPISchema = {
  paths: {
    [path: string]: {
      [method: string]: {
        summary: string;
        operationId: string;
        tags: string[];
      };
    };
  };
};

type OperationItem = {
  id: string;
  icon: { source: Icon; tintColor: Color };
  title: string;
  subtitle: string;
  accessory: { text: string; color: Color };
  url: string;
};

function getMethodIcon(method: string): { source: Icon; tintColor: Color } {
  const color = getMethodColor(method);
  switch (method.toUpperCase()) {
    case "GET":
      return { source: Icon.ArrowDownCircle, tintColor: color };
    case "POST":
      return { source: Icon.ArrowUpCircle, tintColor: color };
    case "PUT":
      return { source: Icon.ArrowUpCircle, tintColor: color };
    case "PATCH":
      return { source: Icon.Pencil, tintColor: color };
    case "DELETE":
      return { source: Icon.Trash, tintColor: color };
    default:
      return { source: Icon.Link, tintColor: color };
  }
}

function getMethodColor(method: string): Color {
  switch (method.toUpperCase()) {
    case "GET":
      return Color.Green;
    case "POST":
      return Color.Blue;
    case "PUT":
      return Color.Orange;
    case "PATCH":
      return Color.Yellow;
    case "DELETE":
      return Color.Red;
    default:
      return Color.SecondaryText;
  }
}

function transformSchemaToItems(data: OpenAPISchema): { [tag: string]: OperationItem[] } {
  const groups: { [tag: string]: OperationItem[] } = {};

  if (data.paths) {
    Object.entries(data.paths).forEach(([path, methods]) => {
      Object.entries(methods).forEach(([method, op]) => {
        const tag = op.tags && op.tags.length > 0 ? op.tags[0] : "Other";
        if (!groups[tag]) groups[tag] = [];
        groups[tag].push({
          id: `${method.toUpperCase()} ${path}`,
          icon: getMethodIcon(method),
          title: op.summary || `${method.toUpperCase()} ${path}`,
          subtitle: path,
          accessory: { text: method.toUpperCase(), color: getMethodColor(method) },
          url: PAGE_URL + op.operationId.toLowerCase(),
        });
      });
    });
  }
  return groups;
}

export default function Command() {
  const [items, setItems] = useState<{ [tag: string]: OperationItem[] }>({});
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    async function fetchData() {
      try {
        setIsLoading(true);
        const res = await fetch(SCHEMA_URL);
        const data = (await res.json()) as OpenAPISchema;
        setItems(transformSchemaToItems(data));
      } catch (e) {
        console.error("Error fetching schema:", e);
        setItems({});
      } finally {
        setIsLoading(false);
      }
    }
    fetchData();
  }, []);

  return (
    <List isLoading={isLoading} searchBarPlaceholder="Search operations...">
      {Object.entries(items).map(([tag, ops]) => (
        <List.Section key={tag} title={tag}>
          {ops.map((item) => {
            const mdUrl = `${item.url}.md`;
            const prompt = `Read ${mdUrl} so I can ask questions about it.`;
            const claudeUrl = `https://claude.ai/?prompt=${encodeURIComponent(prompt)}`;
            const chatgptUrl = `https://chat.openai.com/?prompt=${encodeURIComponent(prompt)}`;
            return (
              <List.Item
                key={item.id}
                icon={item.icon}
                title={item.title}
                subtitle={item.subtitle}
                accessories={[{ tag: { value: item.accessory.text, color: item.accessory.color } }]}
                actions={
                  <ActionPanel>
                    <Action.OpenInBrowser url={item.url} title="Open Docs in Browser" />
                    <Action.CopyToClipboard content={item.url} title="Copy Docs URL" />
                    <Action.OpenInBrowser url={mdUrl} title="Open as Markdown" />
                    <Action.OpenInBrowser url={claudeUrl} title="Ask Claude About This Page" />
                    <Action.OpenInBrowser url={chatgptUrl} title="Ask ChatGPT About This Page" />
                  </ActionPanel>
                }
              />
            );
          })}
        </List.Section>
      ))}
    </List>
  );
}
