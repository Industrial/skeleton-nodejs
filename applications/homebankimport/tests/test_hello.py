"""Hello unit test module."""

from homebankimport.hello import hello


def test_hello():
    """Test the hello function."""
    assert hello() == "Hello homebankimport"
