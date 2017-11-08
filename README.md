

```
/// id type
type id = integer;

type user = struct "User" {
    /// unique id of the user
    id: id,
    name: string?,
    type: enum {"admin", "writer", "reader"},
    SNSs: [string],
};

type group = struct "Group" {
  id: id,
  members: [user],
};
```

```console
$ cargo run etc/test.jsd
```

```json
{
  "group": {
    "properties": {
      "id": {
        "$ref": "#/definition/id"
      },
      "members": {
        "items": {
          "$ref": "#/definition/user"
        },
        "type": "array"
      }
    },
    "required": [
      "id",
      "members"
    ],
    "title": "Group",
    "type": "object"
  },
  "id": {
    "description": "id type",
    "type": "integer"
  },
  "user": {
    "properties": {
      "SNSs": {
        "items": {
          "type": "string"
        },
        "type": "array"
      },
      "id": {
        "$ref": "#/definition/id",
        "description": "unique id of the user"
      },
      "name": {
        "nullable": true,
        "type": "string"
      },
      "type": {
        "enum": [
          "admin",
          "writer",
          "reader"
        ]
      }
    },
    "required": [
      "id",
      "name",
      "type",
      "SNSs"
    ],
    "title": "User",
    "type": "object"
  }
}

```
