

```
/** id type */
type id = integer;

/** @title User */
type user = struct {
    /** unique id of the user */
    id: id,
    name: string?,
    type: enum {"admin", "writer", "reader"},
    SNSs: [string],
};
/**
 * @title Group
 * this expresses a group of users
 */
type group = struct {
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
    "description": "this expresses a group of users",
    "properties": {
      "id": {
        "$ref": "#/definitions/id"
      },
      "members": {
        "items": {
          "$ref": "#/definitions/user"
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
        "$ref": "#/definitions/id",
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
