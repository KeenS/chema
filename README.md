

```
type id = integer;

type user = struct "User" {
    /// unique id of the user
    id: id,
    name: Option<string>,
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
    "type": "object"
  },
  "id": {
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
        "$ref": "#/definition/id"
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
    "type": "object"
  }
}
```
