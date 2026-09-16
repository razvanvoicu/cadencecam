package sgrv.be.sessions

class AccountKeySuite extends munit.FunSuite:

  private val secret = "a-secret-that-lives-outside-the-repository"

  test("the same account always names the same document"):
    assertEquals(AccountKey.name("jane@example.com", secret), AccountKey.name("jane@example.com", secret))

  test("an address that differs only in case or spacing is the same account"):
    val plain = AccountKey.name("jane@example.com", secret)

    assertEquals(AccountKey.name("Jane@Example.com", secret), plain)
    assertEquals(AccountKey.name("  jane@example.com  ", secret), plain)

  test("different accounts name different documents"):
    assertNotEquals(AccountKey.name("jane@example.com", secret), AccountKey.name("john@example.com", secret))

  test("the address cannot be read back out of the name"):
    val name = AccountKey.name("jane@example.com", secret)

    assert(!name.contains("jane"))
    assert(!name.contains("example"))
    assertEquals(name.length, 64)
    assert(name.forall(character => character.isDigit || ('a' to 'f').contains(character)), name)

  test("the key is what makes the name unguessable"):
    // Without this, a name is just a digest of an email: the space of addresses is small enough to walk, so anyone
    // holding the database could confirm who has an account. The secret is not in the database.
    assertNotEquals(AccountKey.name("jane@example.com", secret), AccountKey.name("jane@example.com", "another-secret"))
