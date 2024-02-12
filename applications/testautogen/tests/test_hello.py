"""Hello unit test module."""

from testautogen.hello import hello


def test_hello():
    """Test the hello function."""
    assert hello() == "Hello testautogen"
